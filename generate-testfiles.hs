#!/usr/bin/env stack
-- stack --resolver lts-12.26 script --package directory-tree
import           System.Directory.Tree

testFilesTree :: DirTree String
testFilesTree = Dir
  { name     = "testfiles"
  , contents = [ File "file1" ""
               , File "file2" ""
               , File "file3" ""
               , Dir {name = "dir1", contents = [File "dir1file1" "", File "dir1file2" ""]}
               , Dir
                 { name     = "dir2"
                 , contents = [ File "dir2file1" ""
                              , Dir
                                { name     = "dir2subdir1"
                                , contents = [File "dir2subdir1file1" "", File "dir2subdir1file2" ""]
                                }
                              ]
                 }
               ]
  }

main = writeDirectory $ "." :/ testFilesTree
