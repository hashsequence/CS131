#!/bin/sh
rm -rf testres
javac hw5.java
java test florence.ppm
pnmtojpeg neg.ppm > neg.jpg
pnmtojpeg grey.ppm > grey.jpg
pnmtojpeg mirror.ppm > mirror.jpg
pnmtojpeg mirror2.ppm > mirror2.jpg
pnmtojpeg blur.ppm > blur.jpg
pnmtojpeg blur2.ppm > blur2.jpg
pnmtojpeg blur3.ppm > blur3.jpg
#pnmtojpeg blur3_2.ppm > blur3_2.jpg
pnmtojpeg blur4.ppm > blur4.jpg
pnmtojpeg florence.ppm > florence.jpg
mkdir testres
mv neg.jpg testres/neg.jpg
mv grey.jpg testres/grey.jpg
mv mirror.jpg testres/mirror.jpg
mv mirror2.jpg testres/mirror2.jpg
mv blur.jpg testres/blur.jpg
mv blur2.jpg testres/blur2.jpg
mv blur3.jpg testres/blur3.jpg
#mv blur3_2.jpg testres/blur3_2.jpg
mv blur4.jpg testres/blur4.jpg
mv florence.jpg testres/florence.jpg
rm -rf neg.ppm grey.ppm mirror.ppm mirror2.ppm blur.ppm blur2.ppm blur3.ppm
