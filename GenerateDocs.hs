import Control.Monad
import System.Process
import System.Directory

main = do
	doesDirectoryExist "docs" >>= flip unless (createDirectory "docs")
	putStrLn "Generating documentation..."
	runCommand "haddock Main.hs -h -o docs"
