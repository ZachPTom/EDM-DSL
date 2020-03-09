import EDMDSL

test synthF beatF bpm = do
  putStrLn $ "***Testing " ++ synthF ++ " " ++ beatF
  --showParsedExp synthF
  --showParsedExp beatF
  --Boolean indicates whether the desired Sonic Pi sound is a sample or synth
  runFile synthF bpm False
  runFile beatF bpm True
  putStrLn ""

main :: IO ()
main = do
  test "song.edmdsl" "beat.edmdsl" 220

