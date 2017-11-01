# bestNormalize: Flexibly calculate the best normalizing transformation for a vector

The `bestNormalize` R package was designed to help find a normalizing transformation for a vector. There are many techniques that have been developed in this aim, however each has been subject to some limitations:

- Box Cox transformations work well, but they are only applicable to positive data
- Yeo-Johnson work well for both positive and negative data, but it doesn't work as well as Box Cox for positive data
- Lambert's W works well for some data, but not for all cases


The bottom line: none of these methods are guaranteed to produce the best normalizing transformation. This package will look at a range of possible transformations and return the best one, ie the one that makes it look the *most* normal. 

This package also introduces a technique to normalize the data based off of a smoothed rank mapping to the normal distribution, which allows us to guarantee normally distributed transformed data (if ties are not present). 

