library ("EBImage")
source ("rdfMoments.R")

# Tests to see the image orientation
axis <- calculateMainImageAxis("rdf-rectangle-diagonal.png", normalise=FALSE)
print(axis)
axis <- calculateMainImageAxis("rdf-rectangle-horizontal.png", normalise=FALSE)
print(axis)
axis <- calculateMainImageAxis("rdf-rectangle-vertical.png", normalise=FALSE)
print(axis)