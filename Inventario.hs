{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Time (UTCTime, getCurrentTime)
import System.IO (hFlush, stdout)
import System.Directory (doesFileExist)
import Control.Exception (catch, IOException)
import Data.List (sortBy, foldl', groupBy, maximumBy, isPrefixOf)
import Data.Ord (comparing)
import Data.Char (isSpace)
import Control.Monad (when)

newtype ItemID = ItemID { getID :: String }
  deriving (Show, Read, Eq, Ord)

data Item = Item
  { itemID :: ItemID
  , nome :: String
  , quantidade :: Int
  , categoria :: String
  } deriving (Show, Read, Eq)

data StatusLog = Sucesso | Falha String
  deriving (Show, Read, Eq)

data AcaoLog
  = Add ItemID String Int String
  | Remove ItemID Int
  | Update ItemID Int
  deriving (Show, Read, Eq)

data LogEntry = LogEntry
  { timestamp :: UTCTime
  , acao :: AcaoLog
  , detalhes :: String
  , status :: StatusLog
  } deriving (Show, Read, Eq)

type Inventario = Map ItemID Item
type ResultadoOperacao = (Inventario, LogEntry)

arquivoInv, arquivoLog :: FilePath
arquivoInv = "Inventario.dat"
arquivoLog = "Auditoria.log"

validateNonNeg :: Int -> Either String Int
validateNonNeg n = if n < 0 then Left "quantidade negativa" else Right n

validatePos :: Int -> Either String Int
validatePos n = if n <= 0 then Left "quantidade deve ser maior que zero" else Right n

applyEvent :: AcaoLog -> Inventario -> Inventario
applyEvent (Add k nm q cat) inv =
  case M.lookup k inv of
    Nothing -> M.insert k (Item k nm q cat) inv
    Just it -> if nome it == nm
                 then M.insert k (it { quantidade = quantidade it + q }) inv
                 else M.insert k (it { quantidade = quantidade it + q }) inv
applyEvent (Remove k q) inv =
  case M.lookup k inv of
    Nothing -> inv
    Just it ->
      let newq = quantidade it - q
      in if newq <= 0 then M.delete k inv else M.insert k (it { quantidade = newq }) inv
applyEvent (Update k q) inv =
  case M.lookup k inv of
    Nothing -> M.insert k (Item k "" q "") inv
    Just it -> M.insert k (it { quantidade = q }) inv

allKeys :: Inventario -> [ItemID]
allKeys = M.keys

generateUnique :: ItemID -> Inventario -> ItemID
generateUnique (ItemID base) inv =
  let candidates = [ItemID (base ++ "_" ++ show n) | n <- [1 :: Int ..]]
  in head $ filter (\i -> M.notMember i inv) candidates

prefixMatch :: ItemID -> Inventario -> Either String ItemID
prefixMatch (ItemID pref) inv =
  let keys = allKeys inv
      found = filter (isPrefixOf pref . getID) keys
  in case found of
       [] -> Left "nao achei"
       [x] -> Right x
       _ -> Left "id ambíguo, forneca mais chars"

addItem :: UTCTime -> ItemID -> String -> Int -> String -> Inventario -> Either String ResultadoOperacao
addItem t requestedId nm q cat inv = do
  _ <- validateNonNeg q
  let usedId = if M.member requestedId inv
                 then if maybe False (\it -> nome it == nm) (M.lookup requestedId inv)
                        then requestedId
                        else generateUnique requestedId inv
                 else requestedId
  let evt = Add usedId nm q cat
  let inv2 = applyEvent evt inv
  let lg = LogEntry t evt ("add solicitado: " ++ getID requestedId ++ " -> usado: " ++ getID usedId) Sucesso
  return (inv2, lg)

removeItem :: UTCTime -> ItemID -> Int -> Inventario -> Either String ResultadoOperacao
removeItem t maybeId q inv = do
  _ <- validatePos q
  resolved <- if M.member maybeId inv then Right maybeId else prefixMatch maybeId inv
  case M.lookup resolved inv of
    Nothing -> Left "nao achei depois de resolver"
    Just it -> if quantidade it < q then Left "estoque insuficiente" else do
      let evt = Remove resolved q
      let inv2 = applyEvent evt inv
      let lg = LogEntry t evt ("remove solicitado: " ++ getID maybeId ++ " -> usado: " ++ getID resolved) Sucesso
      return (inv2, lg)

updateItem :: UTCTime -> ItemID -> Int -> Inventario -> Either String ResultadoOperacao
updateItem t maybeId q inv = do
  _ <- validateNonNeg q
  resolved <- if M.member maybeId inv then Right maybeId else prefixMatch maybeId inv
  let evt = Update resolved q
  let inv2 = applyEvent evt inv
  let lg = LogEntry t evt ("update solicitado: " ++ getID maybeId ++ " -> usado: " ++ getID resolved) Sucesso
  return (inv2, lg)

loadInventoryCache :: IO Inventario
loadInventoryCache = do
  ex <- doesFileExist arquivoInv
  if not ex then return M.empty else catch (read <$> readFile arquivoInv) (\(_::IOException) -> return M.empty)

loadLogs :: IO [LogEntry]
loadLogs = do
  ex <- doesFileExist arquivoLog
  if not ex then return [] else catch (parse <$> readFile arquivoLog) (\(_::IOException) -> return [])
  where
    parse txt = foldl' (\acc ln -> case reads ln of {[(v,"")]-> v:acc; _->acc}) [] (lines txt)

saveCache :: Inventario -> IO ()
saveCache inv = writeFile arquivoInv (show inv)

appendLog :: LogEntry -> IO ()
appendLog e = appendFile arquivoLog (show e ++ "\n")

failLog :: UTCTime -> AcaoLog -> String -> LogEntry
failLog t a m = LogEntry t a m (Falha m)

parseArgs :: String -> [String]
parseArgs s = reverse (go [] (dropWhile isSpace s))
  where
    go acc [] = acc
    go acc ('"':xs) = let (w,r) = span (/= '"') xs in go (w:acc) (dropWhile isSpace (drop 1 r))
    go acc xs = let (w,r) = break isSpace xs in go (w:acc) (dropWhile isSpace r)

executeOp :: UTCTime -> Either String ResultadoOperacao -> Inventario -> IO Inventario
executeOp now (Right (inv2, lg)) _ = do
  appendLog lg
  saveCache inv2
  putStrLn "ok"
  return inv2
executeOp now (Left err) inv = do
  let placeholder = Add (ItemID "") "" 0 ""
  let lg = failLog now placeholder err
  appendLog lg
  putStrLn ("erro: " ++ err)
  return inv

listItems :: Inventario -> IO ()
listItems inv = mapM_ showOne (sortBy (comparing itemID) (M.elems inv))
  where
    showOne it = putStrLn $ getID (itemID it) ++ " | " ++ nome it ++ " | qtd: " ++ show (quantidade it) ++ " | cat: " ++ categoria it

showItem :: String -> Inventario -> IO ()
showItem s inv = case M.lookup (ItemID s) inv of
  Nothing -> putStrLn "nao achou"
  Just it -> print it

seed :: IO Inventario
seed = do
  now <- getCurrentTime
  logs <- loadLogs
  let current = rebuild logs
  let candidates = [ItemID ("item" ++ show n) | n <- [1..10], M.notMember (ItemID ("item" ++ show n)) current]
  let evs = [ Add i ("produto " ++ drop 4 (getID i)) ((read (drop 4 (getID i))) * 4) ("cat" ++ show (((read (drop 4 (getID i))) `mod` 3) + 1)) | i <- candidates ]
  let entries = [ LogEntry now e "seed" Sucesso | e <- evs ]
  mapM_ appendLog entries
  logs' <- loadLogs
  let inv' = rebuild logs'
  saveCache inv'
  putStrLn "seed ok"
  return inv'

rebuild :: [LogEntry] -> Inventario
rebuild = foldl' (\acc (LogEntry _ ev _ s) -> if s == Sucesso then applyEvent ev acc else acc) M.empty

groupCount :: Ord a => [a] -> [(a, Int)]
groupCount xs = let g = groupBy (==) (sortBy compare xs) in map (\h -> (head h, length h)) g

idsFromLog :: LogEntry -> [ItemID]
idsFromLog (LogEntry _ ev _ _) =
  case ev of
    Add k _ _ _ -> [k]
    Remove k _ -> [k]
    Update k _ -> [k]

historyByItem :: [LogEntry] -> [String]
historyByItem logs = let ids = concatMap idsFromLog logs; cnt = groupCount ids in map (\(k,n) -> getID k ++ ": " ++ show n ++ " ops") cnt

errorsOnly :: [LogEntry] -> [LogEntry]
errorsOnly = filter (\(LogEntry _ _ _ st) -> case st of {Falha _ -> True; _ -> False})

mostActive :: [LogEntry] -> String
mostActive logs =
  let ids = concatMap idsFromLog logs
  in if null ids then "nenhum"
     else let cnt = groupCount ids; (k,n) = maximumBy (comparing snd) cnt in getID k ++ " (" ++ show n ++ ")"

report :: IO ()
report = do
  logs <- loadLogs
  putStrLn "\n-- Relatorio --"
  putStrLn "\nHistorico por item:"
  mapM_ putStrLn (historyByItem logs)
  putStrLn "\nErros:"
  mapM_ print (errorsOnly logs)
  putStrLn "\nMais movimentado:"
  putStrLn (mostActive logs)
  putStrLn ""

process :: [String] -> Inventario -> IO Inventario
process args inv = do
  now <- getCurrentTime
  case args of
    ("add":kid:nm:q:xs) ->
      case reads q of
        [(qq,"")] -> executeOp now (addItem now (ItemID kid) nm qq (unwords xs) inv) inv
        _ -> putStrLn "qtd invalida" >> return inv
    ("remove":kid:q:_) ->
      case reads q of
        [(qq,"")] -> executeOp now (removeItem now (ItemID kid) qq inv) inv
        _ -> putStrLn "qtd invalida" >> return inv
    ("update":kid:q:_) ->
      case reads q of
        [(qq,"")] -> executeOp now (updateItem now (ItemID kid) qq inv) inv
        _ -> putStrLn "qtd invalida" >> return inv
    ("list":_) -> listItems inv >> return inv
    ("show":x:_) -> showItem x inv >> return inv
    ("seed":_) -> seed
    ("report":_) -> report >> return inv
    ("sair":_) -> putStrLn "encerrando..." >> return inv
    _ -> putStrLn "nao entendi" >> return inv

loop :: Inventario -> IO ()
loop inv = do
  putStr "inventario> "
  hFlush stdout
  l <- getLine
  let p = parseArgs l
  if null p || head p == "sair"
    then do putStrLn "tchau" ; return ()
    else do
      inv2 <- process p inv
      saveCache inv2
      loop inv2

main :: IO ()
main = do
  putStrLn "Sistema de inventario"
  logs <- loadLogs
  let inv = rebuild logs
  saveCache inv
  putStrLn ("itens carregados: " ++ show (M.size inv))
  putStrLn "comandos: add | remove | update | list | show | seed | report | sair"
  loop inv


