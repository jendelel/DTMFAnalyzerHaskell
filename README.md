# DTMFAnalyzerHaskell
DTMF Analyzer in Haskell (School project)

How it works?
-------------
1) The wave file is parsed into array of floats
2) We take a "window" of 100 floats and based on the average absolute value analyse whether there is sound or not
3) Then we take the non-quite intervals and run FFT on them
4) We take the two DTMF frequencies with highest abs value of their coeffiecient
5) Map the frequencies to dictionary and print the character representing them.

Credits:
--------
First of all, I would like to thanks to Martin Mares who helped me with the concept of FFT. Second, thank you, Miran Lipovica, for your great book http://learnyouahaskell.com/. Last but not least, my thanks goes to the creators of these wonderful websites: http://www.dialabc.com/ and https://twistedwave.com/online/.
