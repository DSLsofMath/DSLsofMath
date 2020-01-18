-- Exercise 1.12
module Ex_1_12 where
-- Function from Bool to integer, for testing
tf :: Bool -> Integer
tf True  = 1
tf False = 0

isoR :: (Bool -> t) -> (t, t)
isoR f = (f True, f False)

isoL :: (t, t) -> (Bool -> t)
isoL (a,b) = func
    where
        func True  = a
        func False = b


-- Test the functionality of isoR and isoL
test0,test1,test2 :: Bool
test0 = isoL(isoR tf) True == fst(isoR(isoL (1,0))) && isoL(isoR tf) False == snd(isoR(isoL (1,0)))

-- "isoL◦isoR = id and isoR◦isoL = id" 
test1 = fst (1,0) == fst (isoR (isoL (1,0)))
        && snd (1,0) == snd (isoR (isoL (1,0)))

test2 = tf True == isoL (isoR tf) True 
        && tf False == isoL (isoR tf) False
