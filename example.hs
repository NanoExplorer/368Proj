data Thing = A | B deriving (Show)

flipThing :: Thing -> Thing
flipThing A = B
flipThing B = A
