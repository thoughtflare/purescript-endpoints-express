module EndpointExample.Client where

import Prelude (Unit, unit, pure, show, (<>), ($), bind)

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (message)
import Control.Monad.Aff (runAff, Canceler, nonCanceler)
import Control.Monad.Aff.Endpoint (execEndpoint)

import Network.HTTP.Affjax (AJAX)

import EndpointExample.Model (getOrdersEndpoint)

----------------------------

type CDA a = Canceler (dom :: DOM, ajax :: AJAX | a)
type EffA a = Eff (dom :: DOM, ajax :: AJAX | a)

foreign import data DOM :: !
foreign import appendToBody :: forall eff. 
  String -> 
  EffA eff Unit

main :: forall eff. 
  EffA eff (CDA eff)
main =
  runAff (\e -> appendToBody $ "Error: " <> message e) (\_ -> appendToBody ("Done!")) do
    ordersForOne <- execEndpoint getOrdersEndpoint 1 unit
    ordersForTwo <- execEndpoint getOrdersEndpoint 2 unit
    liftEff $ appendToBody $ "OrdersForOne: " <> show ordersForOne
    liftEff $ appendToBody $ "OrdersForTwo: " <> show ordersForTwo
    pure nonCanceler
