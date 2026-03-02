import qualified Clash.Prelude as C
import qualified Clash.Sized.Vector as Vec
import Core.Core
import Core.Node
import Data.List.Split
import Numeric (readHex)
import Protocols.Uart.Helper
import System.Environment
import Prelude

type ByteNodeSize = (C.BitSize (Maybe (Node PortsNumber AgentType)) C.+ 7) `C.Div` 8

main :: IO ()
main = do
  args <- getArgs
  contents <- readFile $ head args
  let bytes =
        take (C.natToNum @(CellsNumber C.* ByteNodeSize)) $
          map (C.fromInteger . fst . head . readHex) $
            words contents ::
          [Byte]
      chunksSize = C.natToNum @(ByteNodeSize)
      result = map (fromBytes . Vec.unsafeFromList) $ chunksOf chunksSize bytes :: [Maybe (Node PortsNumber AgentType)]

  writeFile (head args ++ "_clash") $ C.show result
