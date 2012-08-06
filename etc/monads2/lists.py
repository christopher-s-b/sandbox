#Prelude> take 5 [[ (i,j) | i <- [1,2]] | j <- [1..]]
#[[(1,1),(2,1)],[(1,2),(2,2)],[(1,3),(2,3)],[(1,4),(2,4)],[(1,5),(2,5)]]

#Prelude> take 5 [ (i,j) | i <- [1..], j <- [1..i-1], gcd i j == 1]
#[(2,1),(3,1),(3,2),(4,1),(4,3)]

#Prelude> take 10 [ (i,j) | i <- [1..], let k = i*i, j <- [1..k]]
#[(1,1),(2,1),(2,2),(2,3),(2,4),(3,1),(3,2),(3,3),(3,4),(3,5)]
