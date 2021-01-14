# Image segmentation

- aim: partition an inmage into regions.
- most algorithms are based on discountiuity and similarity of intensity values.

### Outline
- morphology
    based on shape or texture.
- threasholding
    find region based on pixel intensities.
- region-based
    based on similarity and adjacency
- edge detection:
    based on discontinuities in pixel intensities.

There are real object edges and false edges. The goal is to get the true edge out without being interfered by false edge.

### Edge models
- edge has magnitude and direction
- edge can be classified into three models:
    - step: a clear transition of intensity
    - ramp: a gradual transition of intensity
    - roof: suden change of intensity in a small area.

### Edge operators.
- use first and second derivative filter to detect edge.
    - they reflect the rate of change of intensity
    - we can deduce the information about edges from the rate of change.

- frist derivative
    - thick edge, peak at the real edge.

- second derivative
    - double edge for each edge.
    - zero crossing, the center of thick edge.

### Effect of Noise on Edge Operators.
- edge operators enhance existing noise.
- second derivative is more sensitive to noise than first derivative.
- when an image is very noisy, first derivative can still make a rought profile, but second derivative is almost unrecognizable.

### Edge direction cookbook
1. Image smoothing, goal is to supress as much noise as possible without destroying edges.
2. detection of candidate edges.
3. edge localizatoin. find the edge's position.
4. some edge detection techniques:
    - image gradient
    - marr-hildreth edge detector (LoT, DoG)
    - Canny edge detector


### Image gradient
- ∇(f) = [d(f, x), d(f, y)] = [gx, gy]
- gradient magnitude
    - M(x, y) = sqrt(gx^2 + gy^2)
- gradient direction
    - alpha(x, y) = atan(gy/gx)
- how to find gradient?
    - gx = correlation(wx, f), gy = correlation(wy, f)
    - find M(x, y)
    - find alpha
```
        wx
     |--*----> gx ----+---> M
     |            \  /
f --+|             \/
     |             /\
     |--*----> gy ---+---> alpha
       wy
```
- different filteres
    - robert operator: to detect diagonal edge
    - prewitt operator: simpe 3x3 mask
    - sobel operator: add smoothing on top of prewitt.

### Marr Hildreth Edge Detector
- detect edge of object based on their scale. like to detect large object or small object.
- steps
    1. convolve the image with one of the operator
        - laplacian of the Gaussian function (LoG)
            - a second derivative that cause zero crossing at edges
            - gaussian smooths out details smaller than sigma. Thus, the
              filter size dtermines the scale of oject to detect.
        - difference of Guassians (DoG) // its a fast approximation of LoG
    2. detect the edge at the zero crossing in the convolved image.

- LoG
    - ∇2 . G(x, y) = drev2(G(x, y), x) + drev2(G(x, y), y)
    - maxican hat shape
    - the algorithm:
        - g(x, y) = [∇2 . G(x, y)] * f(x, y), where * is convolution
    - steps:
        1. fitler the input image with n x n gaussian filter
        2. compute laplacian
        3. find zero crossing.
    - benefit:
        - smooth to reduce noise effect.
        - no ringing effects (spaghetti effects)
        - it's second order, thus it works for the change of any direction
        - will produce thin edge rather than thick one produced by sobel.

- DoG
    - approximate LoG by: ∇2 . G(x, y) ~= G(sig1)(x, y) - G(sig2)(x, y), where  sig1 / sig2 ~= 1.6
    - location of the edge is affected by nearby corners.

### Canny edge detector
- optimal for step edges corrupted by white noise.
- objectives:
    1. low error rate: minimize false edges.
    2. edge points should be well localized.
    3. single edge point response.
- it's a method found by using by muti-objectives optimization.
- steps:
    1. smooth the input with gaussian filter
        - convolve image f(x, y) with 2d gaussian to get fs
    2. compute the gradient magnitude and angle images
        - find ∇ fs and alpha(fs)
        - M(x, y) will wide ridges around edges (local maxima).
    3. apply nonmaxima suppression to the gradient magnitude image.
        - goal is to thin the wide ridges in M.
    4. use double thresholding and connectivity analysis to detect link edges.

- nonmaxima thining:
```
a pixel can have four directions outward (only direction vector).
    \ ^ /
    < 0 >
    / V \
find the direction dk that is the closest to alpha(x, y), this is the gradient
normal to the edge.

If M(x, y) is larger than the two neighbors along dk, it is a candidate edge point.
formally, let gN(x, y) bge the nonmaxima suppressed image, then
gN x y
    | M(x, y) < any two neighbors along dk = 0
    | otherwise = M(x, y)
```

- double thresholding & connectivity analysis.
    - step 3 results in both real and non-real edges.
    - use threshold to remove weak edge points.
    - to avoid some weak points along the true edge being removed, use two thresholds:
        - Thigh and T low ar two thresholds.
        - two pass. first pass use Thigh, mark strong true edges and their connectivity
        - second pass mark edge in connectivity and larger than Tlow to true edge.

# Hough Transform: Parametric edge description.
- data fit:
    - construct a curve or a mathematical function that has the best fit to a series of data points possibly subject to constraint.

- some data fitting techniques
    - least square fit (over constraint)
    - random sample consensus (RANSAC)
    - hough transform
        - voting based algorithm
        - take on point at a time and generate all possible solutions. The solution with the max votes become the best fit.

- Hough transform
    - a featrue extration technique
    - aim ato recognize patterns.
    - useful for detecting parametric shapes (lines, circles, elliptical arcs...)
    - image -> edge detection -> edge pixles -> hough transform -> cruve and boundary.

- summary:
    1. it run a voting procedure in a parameter space.
    2. an accumulator space is constructed to record votes.
    3. the required patterns ar efound at local maxima in the accumulator space.

- points in spatial domain and parameter space
    - spatial domain:
        y = mx + b where x, y are variable.
    - parameter space:
        b = -xm + y where b m are variable.
    in parameter space a single point can determine a line.

- each edge point int image space produce a line in parameter space
- if two point are on a line L (defined by m1 b1), their lines intersect in parameter space.
- key idea:
    - all points on a linie in the image will intersect at the same (m1, b1) in the parmeter space.
    - find intersection point == find x1 y1

- polar representation of line: xcos(τ) + ysin(τ) = ρ




