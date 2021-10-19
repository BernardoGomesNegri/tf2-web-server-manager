module LeEncoding(intEncode, intDecode, decodeS, encodeS) where
import Data.Binary
import Data.Int
import Data.Binary.Put
import Data.Binary.Get
import Data.ByteString.Lazy as BL
import Data.ByteString as B

intEncode :: Int32 -> B.ByteString
intEncode = BL.toStrict . runPut . putInt32le

intDecode :: B.ByteString -> Int32
intDecode = runGet getInt32le . BL.fromStrict

decodeS :: Binary a => B.ByteString -> a
decodeS = decode . BL.fromStrict

encodeS :: Binary a => a -> B.ByteString
encodeS = BL.toStrict . encode