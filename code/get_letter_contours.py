import cv2
import numpy as np
import glob
import math
import json
import os
import pandas as pd


def getFileRoot(filename):
	return os.path.basename(filename).split('.')[0]

def main():
    WHITE = [255,255,255]
    filelist = glob.glob('../images/G/*.jpg')

    for name in filelist:
        im = cv2.imread(name)
        newname = '../data/' + getFileRoot(name)

        im = cv2.copyMakeBorder(im,10,10,10,10,cv2.BORDER_CONSTANT,value=WHITE)
        #imnew = 0*im + 255
        imnew = (255 - im)

        # cv2.imshow('image', imnew)
        # cv2.waitKey(0)
        # cv2.destroyAllWindows()

        imgray = cv2.cvtColor(imnew,cv2.COLOR_BGR2GRAY)
        ret,thresh = cv2.threshold(imgray,127,255,0)

        # Remove some small noise if any.
        dilate = cv2.dilate(thresh, None, iterations = 2)
        erode = cv2.erode(dilate, None, iterations = 2)
        #dilate = cv2.dilate(erode, None, iterations = 1)

        cv2.imshow('image', erode)
        cv2.waitKey(0)
        cv2.destroyAllWindows()

        contours, hierarchy = cv2.findContours(dilate, cv2.RETR_TREE, cv2.CHAIN_APPROX_NONE)
        
        contour_df = pd.concat(
        	[pd.DataFrame(newcontours.tolist())[0].apply(pd.Series).assign(cont = i) 
        	for i, newcontours in enumerate(contours)])
        
        contour_df.to_csv(newname)
        print(newname)


if __name__ == '__main__':
    main()
