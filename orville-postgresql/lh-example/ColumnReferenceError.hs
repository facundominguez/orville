{-# OPTIONS_GHC -fplugin=LiquidHaskell #-}

import qualified Data.Int as I
import qualified Data.Text as T
import qualified Orville.PostgreSQL as O

main :: IO ()
main = return ()

{- |
  We would like this user code to fail to compile because 'lengthField'
  refers to the column "length", but no such column exists in the 'Foo'
  table. Presumably that would mean adding liquid types for 'FieldDefinition',
  'TableDefinition', 'SqlMarshaller' and 'SelectOptions' to track which columns
  are present or referred to by expressions, and then making the functions that
  manipulate those types reify those liquid types correctly. Here are the docs
  for the types and functions used in this example.

  * FieldDefinition   - https://hackage.haskell.org/package/orville-postgresql-1.0.0.0/docs/Orville-PostgreSQL.html#t:FieldDefinition
  * TableDefinition   - https://hackage.haskell.org/package/orville-postgresql-1.0.0.0/docs/Orville-PostgreSQL.html#t:TableDefinition
  * SqlMarshaller     - https://hackage.haskell.org/package/orville-postgresql-1.0.0.0/docs/Orville-PostgreSQL.html#t:SqlMarshaller
  * SelectOptions     - https://hackage.haskell.org/package/orville-postgresql-1.0.0.0/docs/Orville-PostgreSQL.html#t:SelectOptions
  * integerField      - https://hackage.haskell.org/package/orville-postgresql-1.0.0.0/docs/Orville-PostgreSQL.html#v:integerField
  * marshallField     - https://hackage.haskell.org/package/orville-postgresql-1.0.0.0/docs/Orville-PostgreSQL.html#v:marshallField
  * mkTableDefinition - https://hackage.haskell.org/package/orville-postgresql-1.0.0.0/docs/Orville-PostgreSQL.html#v:mkTableDefinition
  * fieldLessThan     - https://hackage.haskell.org/package/orville-postgresql-1.0.0.0/docs/Orville-PostgreSQL.html#v:fieldLessThan
  * where_            - https://hackage.haskell.org/package/orville-postgresql-1.0.0.0/docs/Orville-PostgreSQL.html#v:where_
  * findEntitiesBy    - https://hackage.haskell.org/package/orville-postgresql-1.0.0.0/docs/Orville-PostgreSQL.html#v:findEntitiesBy
-}
shouldRaiseColumnError :: O.Orville [Foo]
shouldRaiseColumnError =
  O.findEntitiesBy
    fooTable
    (O.where_ (O.fieldLessThan lengthField 10))

data Foo =
  Foo
    { fooId :: I.Int32
    , fooName :: T.Text
    }

data Bar =
  Bar
    { barId :: I.Int32
    , barLength :: I.Int32
    }

-- TODO: I tried initially to use @listElts ["id", "name"]@ in
-- the specification, unfortunately the liquid haskell parser
-- needs some work to accept the bracket syntax. Therefore, I
-- switched to using singleton sets to construct the set of expected
-- column names.
{-
fooTable ::
    {v:_
    | tdColumnNames v ==
        Set_cup
          (Set_sng "id")
          (Set_sng "name")
    }
-}
-- Anyway, the specification is optional, LH can infer it.
-- During development, however, I did use the above specification
-- to check that fooTable did meet the specification that I
-- intended.

{-@ fooTable :: _ @-}
fooTable :: O.TableDefinition (O.HasKey I.Int32) Foo Foo
fooTable =
  O.mkTableDefinition
    "foo"
    (O.primaryKey idField)
    fooMarshaller

{- fooMarshaller ::
      {v:_
      | smColumnNames v ==
          Set_cup
            (Set_sng "id")
            (Set_sng "name")
      }
-}

{-@ fooMarshaller :: _ @-}
fooMarshaller :: O.SqlMarshaller Foo Foo
fooMarshaller =
  fmap (uncurry Foo)
    (zipSqlMarshaller
      (O.marshallField fooId idField)
      (O.marshallField fooName nameField)
    )

-- TODO: Workaround for missing spec of <*>
{-@
assume zipSqlMarshaller ::
  sma:_ ->
  smb:_ ->
  {v:_
  | smColumnNames v == Set_cup (smColumnNames sma) (smColumnNames smb)
  }
@-}
zipSqlMarshaller ::
    O.SqlMarshaller w a ->
    O.SqlMarshaller w b ->
    O.SqlMarshaller w (a, b)
zipSqlMarshaller sma smb = (,) <$> sma <*> smb

{-
barTable ::
    {v:_
    | tdColumnNames v ==
        Set_cup
          (Set_sng "id")
          (Set_sng "length")
    }
-}

{-@ barTable :: _ @-}
barTable :: O.TableDefinition (O.HasKey I.Int32) Bar Bar
barTable =
  O.mkTableDefinition
    "bar"
    (O.primaryKey idField)
    barMarshaller

-- This is the specification that LH should infer for
-- barMarshaller:
--
-- {-@ barMarshaller ::
--       {v:_
--       | smColumnNames v ==
--           Set_cup
--             (Set_sng "id")
--             (Set_sng "length")
--       }
-- @-}
--
-- At least, it can be substituted for the following specification
-- and verification still passes:

{-@ barMarshaller :: _ @-}
barMarshaller :: O.SqlMarshaller Bar Bar
barMarshaller =
  fmap (uncurry Bar)
    (zipSqlMarshaller
      (O.marshallField barId idField)
      (O.marshallField barLength lengthField)
    )

-- TODO: Why does LH reject I.Int32 but accepts GHC.Int.Int32?
-- {-@ idField :: {v:O.FieldDefinition O.NotNull GHC.Int.Int32 | fdName v == "id" } @-}

-- {-@ idField :: {v:_ | fdName v == "id" } @-}

idField :: O.FieldDefinition O.NotNull I.Int32
idField = O.integerField "id"

nameField :: O.FieldDefinition O.NotNull T.Text
nameField = O.unboundedTextField "name"

lengthField :: O.FieldDefinition O.NotNull I.Int32
lengthField = O.integerField "length"
