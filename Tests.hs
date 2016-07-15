module Tests where

import CreateOverlay

testFile1 = "/home/alexandersgreen/External Drives/Expansion Drive/Samba/Contour/20160420/FI010051.MOV"
tempImages1 = "/home/alexandersgreen/Haskell/ContourGPS/images/FI010051"
outputFile1 = "./Overlay2.mp4"

test1 = createOverlays 2 testFile1 tempImages1 outputFile1

testFile2 = "/home/alexandersgreen/External Drives/Expansion Drive/Samba/Contour/20160604/FILE0008.MOV"
tempImages2 = "/home/alexandersgreen/Haskell/ContourGPS/images/FILE0008"
outputFile2 = "./Overlay3.mp4"

test2 = createOverlays 2 testFile2 tempImages2 outputFile2

testFile3 = "/home/alexandersgreen/External Drives/Expansion Drive/Samba/Contour/20160604/FILE0007.MOV"
tempImages3 = "/home/alexandersgreen/Haskell/ContourGPS/images/FILE0007"
outputFile3 = "./Overlay4.mp4"

test3 = createOverlays 2 testFile3 tempImages3 outputFile3

testFile4 = "/home/alexandersgreen/External Drives/Expansion Drive/Samba/Contour/20160604/FILE0001.MOV"
tempImages4 = "/home/alexandersgreen/Haskell/ContourGPS/images/FILE0001"
outputFile4 = "./Overlay5.mp4"

test4 = createOverlays 2 testFile4 tempImages4 outputFile4

testFile5 = "/home/alexandersgreen/External Drives/Expansion Drive/Samba/Contour/20160604/FILE0002.MOV"
tempImages5 = "/home/alexandersgreen/Haskell/ContourGPS/images/FILE0002"
outputFile5 = "./Overlay6.mp4"

test5 = createOverlays 2 testFile5 tempImages5 outputFile5

testFile6 = "/home/alexandersgreen/External Drives/Expansion Drive/Samba/Contour/20160604/FILE0003.MOV"
tempImages6 = "/home/alexandersgreen/Haskell/ContourGPS/images/FILE0003"
outputFile6 = "./Overlay7.mp4"

test6 = createOverlays 2 testFile6 tempImages6 outputFile6

testFile7 = "/home/alexandersgreen/External Drives/Expansion Drive/Samba/Contour/20160420/FILE0047.MOV"
tempImages7 = "/home/alexandersgreen/Haskell/ContourGPS/images/FILE0047"
outputFile7 = "./Overlay8.mp4"

test7 = createOverlays 2 testFile7 tempImages7 outputFile7

testFile8 = "/home/alexandersgreen/External Drives/Expansion Drive/Samba/Contour/20160420/FILE0048.MOV"
tempImages8 = "/home/alexandersgreen/Haskell/ContourGPS/images/FILE0048"
outputFile8 = "./Overlay9.mp4"

test8 = createOverlays 2 testFile8 tempImages8 outputFile8

testFile9 = "/home/alexandersgreen/External Drives/Expansion Drive/Samba/Contour/20160420/FILE0049.MOV"
tempImages9 = "/home/alexandersgreen/Haskell/ContourGPS/images/FILE0049"
outputFile9 = "./Overlay10.mp4"

test9 = createOverlays 2 testFile9 tempImages9 outputFile9

testFile10 = "/home/alexandersgreen/External Drives/Expansion Drive/Samba/Contour/20160420/FILE0051.MOV"
tempImages10 = "/home/alexandersgreen/Haskell/ContourGPS/images/FILE0051"
outputFile10 = "./Overlay11.mp4"

test10 = createOverlays 2 testFile10 tempImages10 outputFile10

test11 = test8 >> test9 >> test10

test12 = createOverlays 2 "/home/alexandersgreen/51.MOV" "/home/alexandersgreen/Haskell/ContourGPS/images/51" "./Overlay12.mp4"

test13 = mapM_ (\x -> createOverlays 2 ("/home/alexandersgreen/External Drives/Expansion Drive/Samba/Cycling/FILE00" ++ x ++ ".MOV") ("/home/alexandersgreen/Haskell/ContourGPS/images/FILE00" ++ x) ("./FILE00" ++ x ++ "-Overlay.mp4")) ["16","17"]

test14 = mapM_ (\x -> createOverlays 1 ("/home/alexandersgreen/External Drives/Expansion Drive/ContourGPS/Cycling/FILE000" ++ x ++ ".MOV") ("/home/alexandersgreen/Haskell/ContourGPS/images/FILE000" ++ x) ("./FILE000" ++ x ++ "-Overlay.mp4")) ["1","1-2","2-1","3","4"]

test15 = mapM_ (\x -> createOverlays 2 ("/home/alexandersgreen/External Drives/Expansion Drive/ContourGPS/Mont Tremblant/FILE00" ++ x ++ ".MOV") ("/home/alexandersgreen/Haskell/ContourGPS/images/FILE00" ++ x) ("./FILE00" ++ x ++ "-Overlay.mp4")) ["01","02","03","04","05","06","07","08","09","10"]

test16 = mapM_ (\x -> createOverlays 2 ("/home/alexandersgreen/External Drives/Expansion Drive/ContourGPS/Mont Sainte-Anne/FILE0" ++ x ++ ".MOV") ("/home/alexandersgreen/Haskell/ContourGPS/images/FILE0" ++ x) ("./FILE0" ++ x ++ "-Overlay.mp4")) ["083","084","085","086","087","088","089","090","091","092","094","095","096","097","098","099","103","104","105","106"]

test17 = createOverlays 2 "/home/alexandersgreen/37.MOV" "/home/alexandersgreen/Haskell/ContourGPS/images/37" "./37-Overlay.mp4"

test18 = mapM_ (\x -> createOverlays 1 ("/home/alexandersgreen/" ++ x ++ ".MOV") ("/home/alexandersgreen/Haskell/ContourGPS/images/" ++ x) ("./" ++ x ++ "-Overlay.mp4")) ["drive1"]

test19 = mapM_ (\x -> createOverlays 2 ("/home/alexandersgreen/External Drives/Expansion Drive/Samba/Contour/20160604/FILE00" ++ x ++ ".MOV") ("/home/alexandersgreen/Haskell/ContourGPS/images/FILE00" ++ x) ("./FILE00" ++ x ++ "-Overlay.mp4")) ["09","10","11"] 


