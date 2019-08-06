module Tramas where

import Prelude
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary, genericArbitrary)
import Test.QuickCheck.Gen (chooseInt, oneOf, randomSample, suchThat, vectorOf)

import Data.Array (replicate, head, updateAt, (!!), last)
import Data.Maybe (fromJust, Maybe(..))
import Data.String (joinWith)
import Effect (Effect)
import Partial.Unsafe (unsafePartial)
import Data.NonEmpty ((:|))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Effect.Console (log)
import Byte (class Bytes, Byte, bitOne, byteGen, bytes, bytesToInt, intToByte, intToBytes, showByte)

unsafeFromJust :: forall a. Maybe a -> a
unsafeFromJust = unsafePartial fromJust

data Trama =
    EthernetII { preambulo :: PreambuloEthernet,
                 sfd :: SFD,
                 macDestino :: MacDestino,
                 macOrigen :: MacOrigen,
                 type :: Type,
                 payload :: Payload,
                 pad :: Pad,
                 fcs :: FCS }
   | T8023 { preambulo :: Preambulo8023,
             macDestino :: MacDestino,
             macOrigen :: MacOrigen,
             length :: Length,
             payload :: Payload,
             pad :: Pad,
             fcs :: FCS }

derive instance genericTrama :: Generic Trama _

instance bytesFromTrama :: Bytes Trama where
  bytes (EthernetII trama) =
    bytes trama.sfd <>
    bytes trama.macDestino <>
    bytes trama.macOrigen <>
    bytes trama.type <>
    bytes trama.payload <>
    bytes trama.pad <>
    bytes trama.fcs
  bytes (T8023 trama) =
    bytes trama.macDestino <>
    bytes trama.macOrigen <>
    bytes trama.length <>
    bytes trama.payload <>
    bytes trama.pad <>
    bytes trama.fcs

instance arbitraryTrama :: Arbitrary Trama where
  arbitrary = genericArbitrary

type Ejercicio = { enunciado :: String, solucion :: Array String }

ejercicio' :: Effect Trama
ejercicio' = oneSample

ejercicio :: Effect Ejercicio
ejercicio = (\trama -> { enunciado: mostrar trama, solucion: correccion trama }) <$> (oneSample :: Effect Trama)

logEjercicio :: Effect Unit
logEjercicio = do
  trama <- (oneSample :: Effect Trama)
  log (joinWith "\n" $ correccion trama)
  log (joinWith "" $ replicate 20 "\n")
  log (mostrar trama)

logRespuesta :: Trama -> Effect Unit
logRespuesta trama = log $ joinWith "\n" $ correccion trama

class Corregible a where
  correccion :: a -> Array String

correccionConNombreDelCampo :: forall a. Corregible a => String -> a -> Array String
correccionConNombreDelCampo nombreDelCampo campo = map (nombreDelCampo <> _) (correccion campo) 

instance corregibleTrama :: Corregible Trama where
  correccion (EthernetII trama) =
    pure "Tipo de trama: EthernetII" <>
    correccionConNombreDelCampo "Mac destino: " trama.macDestino <>
    correccionConNombreDelCampo "Mac origen: " trama.macOrigen <>
    correccionConNombreDelCampo "Tipo: " trama.type <>
    correccionConNombreDelCampo "Payload: " trama.payload <>
    correccionConNombreDelCampo "Pad: " trama.pad <>
    correccionConNombreDelCampo "FCS: " trama.fcs

  correccion (T8023 trama) =
    pure "Tipo de trama: 802.3" <>
    correccionConNombreDelCampo "Mac destino: " trama.macDestino <>
    correccionConNombreDelCampo "Mac origen: " trama.macOrigen <>
    correccionConNombreDelCampo "Longitud: " trama.length <>
    correccionConNombreDelCampo "Payload: " trama.payload <>
    correccionConNombreDelCampo "Pad: " trama.pad <>
    correccionConNombreDelCampo "FCS: " trama.fcs

else instance corregibleMacAddress :: Corregible MacAddress where
  correccion Broadcast = pure $ mostrar Broadcast <> " - " <> "Broadcast"
  correccion multicast@(Multicast _) = pure $ mostrar multicast <> " - " <> "Multicast"
  correccion unicast@(Unicast _) = pure $ mostrar unicast <> " - " <> "Unicast"

else instance corregibleType :: Corregible Type where
  correccion tipo = pure $ mostrar tipo <> " - " <> genericShow tipo

else instance corregibleLength :: Corregible Length where
  correccion (Length length) = pure $ mostrar (Length length) <> " - " <> show (bytesToInt length)

else instance corregiblePayload :: Corregible Payload where
  correccion (Payload payload) = pure $ "... (muy largo para mostrar)"

else instance corregibleSiSePuedeMostrarComoBytes :: (Bytes a) => Corregible a where correccion = pure <<< mostrar

oneSample :: forall a. Arbitrary a => Effect a
oneSample = randomSample arbitrary <#> (head >>> unsafeFromJust)

mostrar :: forall a. Bytes a => a -> String
mostrar = bytes >>> map showByte >>> joinWith " "

data PreambuloEthernet = PreambuloEthernet -- 7 bytes

instance bytesFromPreambuloEthernet :: Bytes PreambuloEthernet where
    bytes _ = replicate 7 [1, 0, 1, 0, 1, 0, 1, 0]

instance arbitraryPreambuloEthernet :: Arbitrary PreambuloEthernet where
    arbitrary = pure PreambuloEthernet

data SFD = SFD -- 1 byte

instance bytesFromSFD :: Bytes SFD where
  bytes _ = [[1, 0, 1, 0, 1, 0, 1, 1]]

instance arbitrarySFD :: Arbitrary SFD where
  arbitrary = pure SFD

data Preambulo8023 = Preambulo8023 -- 8 bytes

instance bytesFromPreambulo8023 :: Bytes Preambulo8023 where
    bytes _ = replicate 6 [1, 0, 1, 0, 1, 0, 0] <> [[1, 0, 1, 0, 1, 0, 0]]

instance arbitraryPreambulo8023 :: Arbitrary Preambulo8023 where
  arbitrary = pure Preambulo8023

type MacDestino = MacAddress

type MacOrigen = MacAddress

data MacAddress = Broadcast -- 6 bytes
                | Multicast (Array Byte)
                | Unicast (Array Byte)

instance bytesFromMacAddress :: Bytes MacAddress where
    bytes Broadcast = replicate 6 (replicate 8 1)
    bytes (Multicast address) = address
    bytes (Unicast address) = address

instance arbitraryMacAddress :: Arbitrary MacAddress where
    arbitrary = oneOf $ pure Broadcast :| [Multicast <$> multicastAddressGen, Unicast <$> multicastAddressGen]
      where multicastAddressGen = (unsafeFromJust <<< updateAt 1 (intToByte 0x1)) <$> vectorOf 6 byteGen
            unicastAddressGen = (vectorOf 6 byteGen) `suchThat` (\address -> (do
                                                                    secondByte <- address !! 1
                                                                    lastBit <- last secondByte
                                                                    pure lastBit) /= Just bitOne)

data Type = IPv4 -- 2 bytes
          | ARP
          | RARP
          | VLAN
          | IPv6
          | LLC

derive instance genericType :: Generic Type _

instance bytesFromType :: Bytes Type where
  bytes ethernetType = (case ethernetType of
    IPv4 -> 0x0800
    ARP -> 0x0806
    RARP -> 0x8035
    VLAN -> 0x8100
    IPv6 -> 0x86DD
    LLC -> 0x80D5) # intToBytes 2

instance arbitraryType :: Arbitrary Type where
  arbitrary = genericArbitrary

data Length = Length (Array Byte) -- 2 bytes

instance bytesFromLength :: Bytes Length where
  bytes (Length length) = length

instance arbitraryLength :: Arbitrary Length where
  arbitrary = Length <$> (intToBytes 2 <$> chooseInt 0 1500)

data Payload = Payload (Array Byte) -- 46 a 1500 bytes

instance bytesFromPayload :: Bytes Payload where
  bytes (Payload payload) = payload

instance arbitraryPayload :: Arbitrary Payload where
  arbitrary = Payload <$> (chooseInt 46 1500 >>= (\n -> vectorOf n byteGen))

data Pad = Pad (Array Byte) -- 0 a 46 bytes

instance bytesFromPad :: Bytes Pad where
  bytes (Pad pad) = pad

instance arbitraryPad :: Arbitrary Pad where
  arbitrary = Pad <$> (chooseInt 0 46 >>= (\n -> vectorOf n byteGen))

data FCS = FCS (Array Byte) --4 bytes

instance bytesFromFCS :: Bytes FCS where
  bytes (FCS fcs) = fcs

instance arbitraryFCS :: Arbitrary FCS where
  arbitrary = FCS <$> vectorOf 4 byteGen