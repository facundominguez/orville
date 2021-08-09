module Test.SelectOptions
  ( selectOptionsTests,
  )
where

import qualified Data.ByteString.Char8 as B8
import qualified Data.Int as Int
import qualified Data.List.NonEmpty as NEL
import qualified Data.String as String
import qualified Hedgehog as HH

import qualified Orville.PostgreSQL.Internal.Expr as Expr
import qualified Orville.PostgreSQL.Internal.FieldDefinition as FieldDef
import qualified Orville.PostgreSQL.Internal.RawSql as RawSql
import qualified Orville.PostgreSQL.Internal.SelectOptions as SO
import qualified Orville.PostgreSQL.Internal.SqlValue as SqlValue
import qualified Test.Property as Property

selectOptionsTests :: IO Bool
selectOptionsTests =
  HH.checkParallel $
    HH.Group
      (String.fromString "SelectOptions")
      [
        ( String.fromString "emptySelectOptions yields no whereClause"
        , Property.singletonProperty $
            assertWhereClauseEquals
              Nothing
              SO.emptySelectOptions
        )
      ,
        ( String.fromString "fieldEquals generates expected sql"
        , Property.singletonProperty $
            assertWhereClauseEquals
              (Just "WHERE (foo = $1)")
              (SO.where_ $ SO.fieldEquals fooField 0)
        )
      ,
        ( String.fromString "fieldNotEquals generates expected sql"
        , Property.singletonProperty $
            assertWhereClauseEquals
              (Just "WHERE (foo <> $1)")
              (SO.where_ $ SO.fieldNotEquals fooField 0)
        )
      ,
        ( String.fromString "fieldLessThan generates expected sql"
        , Property.singletonProperty $
            assertWhereClauseEquals
              (Just "WHERE (foo < $1)")
              (SO.where_ $ SO.fieldLessThan fooField 0)
        )
      ,
        ( String.fromString "fieldGreaterThan generates expected sql"
        , Property.singletonProperty $
            assertWhereClauseEquals
              (Just "WHERE (foo > $1)")
              (SO.where_ $ SO.fieldGreaterThan fooField 0)
        )
      ,
        ( String.fromString "fieldLessThanOrEqualTo generates expected sql"
        , Property.singletonProperty $
            assertWhereClauseEquals
              (Just "WHERE (foo <= $1)")
              (SO.where_ $ SO.fieldLessThanOrEqualTo fooField 0)
        )
      ,
        ( String.fromString "fieldGreaterThanOrEqualTo generates expected sql"
        , Property.singletonProperty $
            assertWhereClauseEquals
              (Just "WHERE (foo >= $1)")
              (SO.where_ $ SO.fieldGreaterThanOrEqualTo fooField 0)
        )
      ,
        ( String.fromString "whereAnd generates expected sql"
        , Property.singletonProperty $
            assertWhereClauseEquals
              (Just "WHERE ((foo = $1) AND (bar = $2))")
              (SO.where_ $ SO.whereAnd (SO.fieldEquals fooField 10 NEL.:| [SO.fieldEquals barField 20]))
        )
      ,
        ( String.fromString "whereOr generates expected sql"
        , Property.singletonProperty $
            assertWhereClauseEquals
              (Just "WHERE ((foo = $1) OR (bar = $2))")
              (SO.where_ $ SO.whereOr (SO.fieldEquals fooField 10 NEL.:| [SO.fieldEquals barField 20]))
        )
      ,
        ( String.fromString "combining SelectOptions ANDs the where clauses together"
        , Property.singletonProperty $
            assertWhereClauseEquals
              (Just "WHERE (foo = $1) AND (bar = $2)")
              ( SO.where_ (SO.fieldEquals fooField 10)
                  <> SO.where_ (SO.fieldEquals barField 20)
              )
        )
      ,
        ( String.fromString "whereIn generates expected sql"
        , Property.singletonProperty $
            assertWhereClauseEquals
              (Just "WHERE (foo IN ($1))")
              (SO.where_ $ SO.whereIn fooField (SqlValue.fromInt32 10 NEL.:| []))
        )
      ,
        ( String.fromString "whereNotIn generates expected sql"
        , Property.singletonProperty $
            assertWhereClauseEquals
              (Just "WHERE (foo NOT IN ($1, $2))")
              (SO.where_ $ SO.whereNotIn fooField (SqlValue.fromInt32 10 NEL.:| [SqlValue.fromInt32 20]))
        )
      ,
        ( String.fromString "distinct generates expected sql"
        , Property.singletonProperty $
            assertDistinctEquals
              (Just "SELECT DISTINCT ")
              (SO.distinct)
        )
      ,
        ( String.fromString "orderBy generates expected sql"
        , Property.singletonProperty $
            assertOrderByClauseEquals
              (Just "ORDER BY foo ASC, bar DESC")
              ( SO.orderBy . Expr.orderByColumnsExpr $
                  (FieldDef.fieldColumnName fooField, Expr.ascendingOrder)
                    NEL.:| [(FieldDef.fieldColumnName barField, Expr.descendingOrder)]
              )
        )
      ,
        ( String.fromString "orderBy generates expected sql with multiple selectOptions"
        , Property.singletonProperty $
            assertOrderByClauseEquals
              (Just "ORDER BY foo ASC, bar DESC")
              ( (SO.orderBy $ Expr.orderByExpr (RawSql.fromString "foo") Expr.ascendingOrder)
                  <> (SO.orderBy $ Expr.orderByExpr (RawSql.toRawSql $ FieldDef.fieldColumnName barField) Expr.descendingOrder)
              )
        )
      ,
        ( String.fromString "groupBy generates expected sql"
        , Property.singletonProperty $
            assertGroupByClauseEquals
              (Just "GROUP BY foo, bar")
              ( SO.groupBy . Expr.groupByColumnsExpr $
                  FieldDef.fieldColumnName fooField NEL.:| [FieldDef.fieldColumnName barField]
              )
        )
      ,
        ( String.fromString "groupBy generates expected sql with multiple selectOptions"
        , Property.singletonProperty $
            assertGroupByClauseEquals
              (Just "GROUP BY foo, bar")
              ( (SO.groupBy . Expr.groupByExpr $ RawSql.fromString "foo")
                  <> (SO.groupBy . Expr.groupByExpr . RawSql.toRawSql $ FieldDef.fieldColumnName barField)
              )
        )
      ]

assertDistinctEquals :: HH.MonadTest m => Maybe String -> SO.SelectOptions -> m ()
assertDistinctEquals mbDistinct selectOptions =
  fmap RawSql.toBytes (SO.selectDistinct selectOptions) HH.=== fmap B8.pack mbDistinct

assertWhereClauseEquals :: HH.MonadTest m => Maybe String -> SO.SelectOptions -> m ()
assertWhereClauseEquals mbWhereClause selectOptions =
  fmap RawSql.toBytes (SO.selectWhereClause selectOptions) HH.=== fmap B8.pack mbWhereClause

assertOrderByClauseEquals :: HH.MonadTest m => Maybe String -> SO.SelectOptions -> m ()
assertOrderByClauseEquals mbOrderByClause selectOptions =
  fmap RawSql.toBytes (SO.selectOrderByClause selectOptions) HH.=== fmap B8.pack mbOrderByClause

assertGroupByClauseEquals :: HH.MonadTest m => Maybe String -> SO.SelectOptions -> m ()
assertGroupByClauseEquals mbGroupByClause selectOptions =
  fmap RawSql.toBytes (SO.selectGroupByClause selectOptions) HH.=== fmap B8.pack mbGroupByClause

fooField :: FieldDef.FieldDefinition FieldDef.NotNull Int.Int32
fooField =
  FieldDef.integerField "foo"

barField :: FieldDef.FieldDefinition FieldDef.NotNull Int.Int32
barField =
  FieldDef.integerField "bar"
