import Datas
import Evaluation

main :: IO ()
main = do
    print $ evalExpr (List [Symbol "+", Number 10, List[Symbol "*", Number 6, Number 2]])