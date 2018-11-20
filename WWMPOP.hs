{-Andrew Wallace
ID: 10642754
CSci 450-01: Org. of Programming Languages
Assignment 2-}

module WWMPOP
where

type BarCode = Int
type Price = Int
type Name = String

type PriceList = [(BarCode,Name,Price)]

type CartItems = [BarCode]
type CartPrices = [(Name,Price)]
type Bill = (CartPrices, Price, Price, Price)

taxRate :: Double
taxRate = 0.07

lineWidth :: Int
lineWidth = 34

database :: PriceList
database = [ (1848, "Vanilla yogurt cups (4)",    188),
                 (1620, "Ground turkey (1 lb)",       316), 
                 (1492, "Corn flakes cereal",         299), 
                 (1773, "Black tea bags (100)",       307), 
                 (2525, "Athletic socks (6)",         825), 
                 (9595, "Claw hammer",                788), 
                 (1945, "32-in TV",                 13949), 
                 (1066, "Zero sugar cola (12)",       334),
                 (2018, "Haskell programming book",  4495)
               ]
--takes in an int and adds the period for cents
formatDollars :: Price -> String
formatDollars p          =
              let p' = show p in
              let p'' = splitAt ((length p' - 2)) p' in
              fst(p'') ++ "." ++ snd(p'')

--generates a string to represent an item purchased
formatLine :: (Name, Price) -> String
formatLine x            =
           let x' = formatDollars(snd (x)) in
           fst(x) ++ addPeriods(lineWidth - (length(x') + length(fst(x)))) ++ x' ++ "\n"
--recursively calls formatLine and itself to generate the list of cartprices
formatLines :: CartPrices -> String
formatLines []               = []
formatLines (x:xs)           = formatLine(x) ++ formatLines(xs)
addPeriods :: Int -> String
addPeriods x           
    | x > 0        = "." ++ addPeriods(x-1)
    | otherwise    = ""
--takes in cartprices and returns the sum
calcSubtotal :: CartPrices -> Price
calcSubtotal p     = 
                   let p' = unzip p in
                   let p'' = snd(p') in
                   sum p''
--generates a string to represent the subtotal, tax and total
formatAmt :: String -> Price -> String
formatAmt tot p          = "\n" ++ tot ++ addPeriods((lineWidth - 1) - (length(tot) + length(show p))) ++ formatDollars (p)
--generates the receipt format from a bill
formatBill :: Bill -> String
formatBill b         =
           let b' = formatLines(fst''(b)) in
           b' ++ (formatAmt "Subtotal" (snd''(b))) ++ (formatAmt "Tax" (thd''(b))) ++ (formatAmt "Total" (frt''(b)))
--receives and pricelist and barcode and returns the (name, price)
look :: PriceList -> BarCode -> (Name,Price)
look p c                = isBar c p
isBar :: BarCode -> PriceList -> (Name,Price)--determines if a barcode is in the pricelist
isBar b [] = ("None",0)
isBar b (x:xs) = if b == (ifst'(x)) then (isnd'(x),ithd'(x))
                      else isBar b xs
--recursively calls itself and look to determine if an item in CartItems is in the pricelist
priceCart :: PriceList -> CartItems -> CartPrices
priceCart price []         = []
priceCart price (x:xs)     = look price x : priceCart price xs
--uses calcSubtotal to generate the bill
makeBill :: CartPrices -> Bill
makeBill c              = 
                        let sub = calcSubtotal c in
                        let tax = convertTax sub in
                        let total = sub + tax in
                        (c, sub, tax, total)
--filters out the . created by multiplying an int by a double
convertTax :: Int -> Price
convertTax x         = 
                     let y = fromIntegral x * taxRate in
                     let z = show y in
                     let tax = filter (not . (`elem` ".")) z in
                     read tax
--builds the walley world receipt
makeReceipt :: PriceList -> CartItems -> String
makeReceipt p c            =
                           let b = makeBill(priceCart p c) in
                           "     "++"Wally World MarketPlace" ++ "\n" ++ "\n" ++ (formatBill b)

--for triples
fst' :: (a, a, a) -> a
fst' (x,_,_) = x
snd' :: (a, a, a) -> a
snd' (_,x,_) = x
thd' :: (a, a, a) -> a
thd' (_,_,x) = x
--for pricelist
ifst' :: (BarCode, Name, Price) -> BarCode
ifst' (x,_,_) = x
isnd' :: (BarCode, Name, Price) -> Name
isnd' (_,x,_) = x
ithd' :: (BarCode, Name, Price) -> Price
ithd' (_,_,x) = x
--for cartprices
fst'' :: (CartPrices, Int, Int, Int) -> CartPrices
fst'' (x,_,_,_) = x
snd'' :: (CartPrices, Int, Int, Int) -> Int
snd'' (_,x,_,_) = x
thd'' :: (CartPrices, Int, Int, Int) -> Int
thd'' (_,_,x,_) = x
frt'' :: (CartPrices, Int, Int, Int) -> Int
frt'' (_,_,_,x) = x