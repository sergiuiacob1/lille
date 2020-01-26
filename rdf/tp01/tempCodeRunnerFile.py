from rdfMoments import *

# Tests to see the image orientation
def checkRectanglesMainAxis():
    axis = calculateMainImageAxis("rdf-rectangle-diagonal.png", normalise=False)
    print ("Diagonal:")
    print(axis)
    axis = calculateMainImageAxis("rdf-rectangle-horizontal.png", normalise=False)
    print ("Horizontal:")
    print(axis)
    axis = calculateMainImageAxis("rdf-rectangle-vertical.png", normalise=False)
    print ("Vertical:")
    print(axis)

def checkSquaresMainAxis():
# pour les deux carres ce dessous, l´inertion est la meme
    axis = calculateMainImageAxis("rdf-carre-6.png", normalise=False)
    print ("Small square:")
    print(axis)
    axis = calculateMainImageAxis("rdf-carre-10.png", normalise=False)
    print ("Big square:")
    print(axis)

checkSquaresMainAxis()