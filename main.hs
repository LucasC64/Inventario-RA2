import Data.Map (Map)
import qualified Data.Map as Map
import Data.Time (UTCTime)

{- Aluno 1: Arquiteto de Dados -}

data Item = Item
  { itemID :: String
  , nome :: String
  , quantidade :: Int
  , categoria :: String
  } deriving (Show, Read, Eq)

type Inventario = Map String Item

data AcaoLog = Add | Remove | Update | QueryFail
  deriving (Show, Read, Eq)

data StatusLog = Sucesso | Falha String
  deriving (Show, Read, Eq)

data LogEntry = LogEntry
  { timestamp :: UTCTime
  , acao :: AcaoLog
  , detalhes :: String
  , status :: StatusLog
  } deriving (Show, Read, Eq)

  

{-Aluno 2: Lógica de Negócio Pura -}

addItem :: Inventario -> Item -> Inventario
addItem inventario novoItem
    | Map.member (itemID novoItem) inventario = inventario 
    | otherwise = Map.insert (itemID novoItem) novoItem inventario


removeItem :: Inventario -> Item -> Inventario
removeItem inventario item
    | Map.member (itemID item) inventario = Map.delete (itemID item) inventario
    | otherwise = inventario


updateQty :: Inventario -> Item -> Inventario
updateQty inventario idItem novaQtd
    | novaQtd < 0 = inventario
    | otherwise   = Map.adjust atualizar idItem inventario
  where
    atualizar item = item { quantidade = novaQtd }



main :: IO ()
    main = do
