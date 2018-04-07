{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Tables.User where

import           AppPrelude
import           Control.Lens                         hiding (element)
import qualified Crypto.Scrypt                        as S
import           Data.UUID                            (UUID)
import           Database.Beam
import           Database.Beam.Backend.SQL.SQL92
import           Database.Beam.Postgres
import           Database.PostgreSQL.Simple.FromField
import           GHC.Generics                         (Generic)
import           Models.Credentials                   (Email (..))
import           Prelude                              (String)

type UserID = UUID

data UserT f
    = User
    { _userId       :: Columnar f UserID
    , _userEmail    :: Columnar f Text
    , _userPassword :: Columnar f S.EncryptedPass
    } deriving (Generic)

type User = UserT Identity

makeLenses ''UserT

deriving instance Generic S.EncryptedPass
instance Beamable UserT
instance Table UserT where
  data PrimaryKey UserT f = UserId (Columnar f UUID) deriving Generic
  primaryKey = UserId . _userId

instance Beamable (PrimaryKey UserT)
deriving instance Show User
instance FromField S.EncryptedPass where
  fromField field mb_bytestring = S.EncryptedPass <$> fromField field mb_bytestring

instance FromField Email where
  fromField field mb_bytestring = Email <$> fromField field mb_bytestring
deriving instance FromBackendRow Postgres S.EncryptedPass
deriving instance FromBackendRow Postgres Email

instance HasSqlValueSyntax be Prelude.String => HasSqlValueSyntax be Email where
  sqlValueSyntax (Email email) = autoSqlValueSyntax email

instance HasSqlValueSyntax be Prelude.String => HasSqlValueSyntax be S.EncryptedPass where
  sqlValueSyntax = autoSqlValueSyntax
