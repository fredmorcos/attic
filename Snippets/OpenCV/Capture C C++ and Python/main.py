#!/usr/bin/python2

import cv
import cv2

capture = cv2.VideoCapture(0)

frame = None
edges = None
hand  = None

while True:
    retval, frame = capture.read()

    # if not retval:
    #     break

    # cv2.imshow("window1", frame)

    edges = cv2.cvtColor(frame, cv.CV_BGR2GRAY)
    edges = cv2.GaussianBlur(edges, (7, 7), sigmaX = 1.5, sigmaY = 1.5)
    edges = cv2.Canny(edges, 0, 30, apertureSize = 3)

    # cv2.rectangle(edges, (20 * edges.size.width / 100, 0),
    #               ((20 * edges.size.width / 100) + (50 * edges.size.width / 100), edges.size.width),
    #               100, 3)
    cv2.rectangle(edges, (10, 10), (50, 50), 100, 3)
    cv2.imshow("window2", edges)

    if cv2.waitKey(5) == 27:
        break

retval, hand = cv2.threshold(edges, 128, 255, cv.CV_THRESH_BINARY)
cv2.destroyWindow("window2")

while True:
    cv2.imshow("window3", hand)

    if cv2.waitKey(5) == 27:
        break

cv2.destroyAllWindows()
