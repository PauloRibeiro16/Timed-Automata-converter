module Main where

import ParsingUtils
import RegularExpression 
import Text.Parsec.Error
import Parser
import Draw
import Graphics.Gloss
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Selectors.FileChooser
import System.Directory

main :: IO ()
main = do
  -- | 
   initGUI
   window <- windowNew
   set window [windowTitle := "File Manager", 
               windowDefaultWidth := 500,
               windowDefaultHeight := 400 ]

   fch <- fileChooserWidgetNew FileChooserActionOpen
   containerAdd window fch 

   selopt <- checkButtonNewWithLabel "Multiple File Selection"
   fileChooserSetExtraWidget fch selopt
  
  -- | Filter all files that end with .txt
   hsfilt <- fileFilterNew
   fileFilterAddPattern hsfilt "*.txt"
   fileFilterSetName hsfilt "files .txt"   
   fileChooserAddFilter fch hsfilt
  -- | Shows all files
   nofilt <- fileFilterNew
   fileFilterAddPattern nofilt "*.*"
   fileFilterSetName nofilt "All Files"
   fileChooserAddFilter fch nofilt

   onFileActivated fch $ 
        do dir <- fileChooserGetCurrentFolder fch
           case dir of 
                Just dpath -> putStrLn 
                               ("The current directory is: " ++ dpath)
                Nothing -> putStrLn "Nothing" 
           mul <- fileChooserGetSelectMultiple fch 
           if mul 
              then do
                fls <- fileChooserGetFilenames fch
                putStrLn 
                  ("You selected " ++ (show (length fls)) ++ "files:")
                sequence_ (map putStrLn fls)
              else do
                file <- fileChooserGetFilename fch
                case file of
                     Just file -> do
                        input <- readFile file
                        result <- (return. parseFile) input  
                        let a = tAutToExpr(result)
                        let final = tAutToExpr(result)>>=pretty'
                        let fe = tAutToExpr(result)>>=structDr
                        print final
                        case final of 
                                Left x -> do
                                  let err = "Error:"++"\n"++(show x)
                                  writeFile "error.txt" err 
                                Right x -> case fe of
                                        Left y ->  print $ y
                                        Right y ->display
                                                            ( FullScreen )        -- window position
                                                            white                    -- background color
                                                            (pictures ([x]++[y]))          -- picture to d
                     Nothing -> putStrLn "Nothing"

   onToggled selopt $ do state <- toggleButtonGetActive selopt
                         fileChooserSetSelectMultiple fch state

   widgetShowAll window
   onDestroy window mainQuit
   mainGUI
