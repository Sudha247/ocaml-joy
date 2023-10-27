import numpy as np
from joy import *


class Ellipse:
    def __init__(self, center, radius_x, radius_y):
        self.center = np.array(center)
        self.radius_x = radius_x
        self.radius_y = radius_y

    def rotate(self, angle):
        # Translate to origin
        translated_center = np.array([0, 0])
        translated_center[0] = self.center[0] - self.radius_x
        translated_center[1] = self.center[1] - self.radius_y

        # Rotate around the origin
        rotated_center = self.rotate_point(translated_center, angle)

        # Translate back to the original center
        self.center[0] = rotated_center[0] + self.radius_x
        self.center[1] = rotated_center[1] + self.radius_y

    @staticmethod
    def rotate_point(point, angle):
        x_rot = point[0] * np.cos(angle) - point[1] * np.sin(angle)
        y_rot = point[0] * np.sin(angle) + point[1] * np.cos(angle)
        return np.array([x_rot, y_rot])
