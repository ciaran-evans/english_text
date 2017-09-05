import cv2
import numpy as np
import glob
import math
#from itertools import product
#im = cv2.imread('character8.jpg')
#print im
#cv2.imshow('image',im)
#cv2.waitKey(0)
#cv2.destroyAllWindows()

def main():
    WHITE = [255,255,255]
    filelist = glob.glob('../images/G/*.jpg')

    for name in filelist:
        im = cv2.imread(name)
        newname = name.replace('G', 'contour_G')

        im = cv2.copyMakeBorder(im,10,10,10,10,cv2.BORDER_CONSTANT,value=WHITE)
        imnew = 0*im + 255
        imgray = cv2.cvtColor(im,cv2.COLOR_BGR2GRAY)
        ret,thresh = cv2.threshold(imgray,127,255,0)

        # Remove some small noise if any.
        erode = cv2.erode(thresh, None, iterations = 2)
        dilate = cv2.dilate(erode, None, iterations = 2)
        contours, hierarchy = cv2.findContours(dilate,cv2.RETR_TREE,cv2.CHAIN_APPROX_TC89_KCOS)
        cv2.drawContours(imnew,contours,-1,(0,0,0),1)
        # cv2.imshow('image', imnew)
        # cv2.waitKey(0)
        # cv2.destroyAllWindows()
        cv2.imwrite(newname, imnew)


if __name__ == '__main__':
    main()
