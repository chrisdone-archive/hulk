module StringProcPlugin (resource) where

import StringProcAPI (plugin,Interface(..))

import Hulk.Types

resource = plugin {
     stringProcessor = \env _conn _line -> return ([Close],env)
}