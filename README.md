## Package imrchart - Individuals and Moving Range Chart ##

**imrchart** is an R package for creating an Individuals and Moving Range Chart from a single numeric data vector.

### Based on Shewart's Control Chart's###

From the book:

Wheeler, Donald J., and David S. Chambers. *Understanding Statistical Process Control*. Knoxville, TN: SPC, 1992. Print.

### Uses the Western Electric Control Chart Tests ###

1. Points above the upper control limit (UCL) or below the lower control limit (LCL).
2. n out of n + 1 points outside 2 standard deviations. n defaults to 2.
3. n out of n + 1 points outside 1 standard deviation. n defaults to 4.
4. n points in a row, increasing or decreasing. n defaults to 6.
5. n points in a row, either side of the center line. n defaults to 9.
6. n points in a row beyond 1 standard deviation. n defaults to 8.
7. n points in a row, alternating up and down. n defaults to 14.
8. n points in a row within 1 standard deviation. n defaults to 15.

The values of n for tests 2 through 8 are passed to the function using the pTs variable. 

The point that causes a violation of the rule is the only point plotted for that violation. If there is more than one violation for a particular point, the highest numbered rule prevails.

Tests may be viewed or ignored using the bTests variable. This is a vector of 8 logical values for each of the tests. TRUE = evaluate the test, FALSE = do not evaluate the test. Default is TRUE for all tests.

### Control Limit Basis ###

use the logical variable bMed to indicate whether the control limits are based on the median moving range (bMed = TRUE) or the mean moving range (bMed = FALSE). Default = FALSE.

