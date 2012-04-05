
module Output where

import Ast

writeModules :: FilePath -> [Module] -> IO ()
writeModules name = writeFile name . const ""

