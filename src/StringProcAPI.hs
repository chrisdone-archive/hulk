module StringProcAPI (Interface(..), plugin, Handler) where

import Hulk.Types

data Interface = Interface {
       stringProcessor :: Handler
}

type Handler = Env -> Conn -> String -> HulkIO ([Reply],Env)

plugin :: Interface
plugin = Interface { 
           stringProcessor = \env _conn _line -> return ([],env)
         }
