# ContourGPS
Haskell code for creating overlays for ContourGPS videos, using ffmpeg

The ContourGPS is a helmet camera that is capable of adding GPS location information to the videos that it records. 
This information is stored as NMEA sentences in the subtitle track of the video.

This project is for extracting those sentences, creating graphical overlays from this data, and overlaying this onto the original video.
The extraction of the subtitles, and the generation of the new video are done using ffmpeg. 
Haskell is used to translate the NMEA sentences into the graphical overlay.

An example video can be found here: https://youtu.be/Dx2LO_MUs-4

