module Main (main) where

main :: IO ()
main = do
    putStrLn "欢迎来到 Haskell 第一课！"
    putStrLn "Welcome to Haskell Lecture 1!"
    putStrLn ""
    putStrLn "请在 GHCi 中加载以下模块进行练习："
    putStrLn "Please load the following modules in GHCi to practice:"
    putStrLn ""
    putStrLn "  :load src/BasicDrills.hs         -- 基础练习"
    putStrLn "  :load src/Lecture1.hs            -- 核心练习"
    putStrLn "  :load src/InteractiveExamples.hs -- 互动示例"
    putStrLn "  :load src/Challenges.hs          -- 挑战题"
    putStrLn ""
    putStrLn "祝学习愉快！Happy learning!"
