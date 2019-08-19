fn square_loop(mut x: f64) {
    loop {
        x = x * x;
        //println!("{}", x);
    }
}

fn square_add_loop(c: f64) {
    let mut x = 0.;
    loop {
        x = x * x + c;
        //println!("{}", x);
    }
}

extern crate num;
use num::Complex;

/// Try to determine if `c` is in the Mandebrot set, using at most `limit` iterations to decide.
///
/// If `c` is not a member, return `Some(i)`, where `i` is the number of
/// iterations it took for `c` to leave the circle of radius two centered on the
/// origin. If `c` seems to be a member (more precisely, if we reached the
/// iteration limit without being able to prove that `c` is not a member)
/// return `None`.
fn escape_time(c: Complex<f64>, limit: u32) -> Option<u32> {
    let mut z = Complex { re: 0.0, im: 0.0 };
    for i in 0..limit {
        z = z * z + c;
        if z.norm_sqr() > 4.0 {
            return Some(i);
        }
    }

    None
}

use std::str::FromStr;

/// Parse the string `s` as a coodinate pair, like `"400x600"` or `"1.0,0.5"`.
///
/// Specifically, `s` should have the form <left><sep><right>, where <sep> is
/// the character given by the `separator` argument, and <left> and <right> are both
/// strings that can be parsed by `T::from_str`.
///
/// ```rust
/// assert_eq!(parse_pair("400x600", 'x'), Some((400, 600)));
/// assert_eq!(parse_pair("1.0,0.5", ','), Some((1.0, 0.5)));
/// ```
///
/// If `s` has the proper form, return `Some<(x,y)>`. If it doesn't parse
/// correctly, return `None`.
///
/// ```rust
/// assert_eq!(parse_pair("400 600", 'x'), None);
/// assert_eq!(parse_pair("400x600", ','), None);
/// ```
pub fn parse_pair<T: FromStr>(s: &str, separator: char) -> Option<(T,T)> {
    match s.find(separator) {
        None => None,
        Some(index) => {
            match (T::from_str(&s[..index]), T::from_str(&s[index+1..])) {
                (Ok(l), Ok(r)) => Some((l,r)),
                _ => None
            }
        }
    }
}

#[test]
fn test_parse_pair() {
    assert_eq!(parse_pair("400x600", 'x'), Some((400, 600)));
    assert_eq!(parse_pair("1.0,0.5", ','), Some((1.0, 0.5)));
    assert_eq!(parse_pair::<i32>("400 600", 'x'), None);
    assert_eq!(parse_pair::<i32>("400x600", ','), None);
}

/// Parse a pair of floating-point numbers separeted by a comma as a complex number.
fn parse_complex(s: &str) -> Option<Complex<f64>> {
    match parse_pair(s, ',') {
        Some((re, im)) => Some(Complex::new(re, im)),
        _ => None
    }
}

#[test]
fn test_parse_complex() {
    assert_eq!(parse_complex("1,0"),   Some(Complex{re:1., im:0.}));
    assert_eq!(parse_complex("1.1,3"), Some(Complex{re:1.1, im:3.}));
    assert_eq!(parse_complex("-1,-1"), Some(Complex{re:-1., im:-1.}));
    assert_eq!(parse_complex(""),      None);
    assert_eq!(parse_complex("1"),     None);
    assert_eq!(parse_complex("3.14,"), None);
    assert_eq!(parse_complex(",0"),    None);
}

/// Given the row and column of a pixel in the output image, return the
/// corresponding point on the complex plane.
///
/// `bounds` is a pair giving the witdth and height of the image in pixels.
/// `pixel` is a (column, row) pair indicating a particular pixel in that image.
/// The `upper_left` and `lower_right` parameter are points on the complex
/// plane designating the area our image covers.
fn pixel_to_point(bounds: (usize, usize),
                  pixel: (usize, usize),
                  upper_left: Complex<f64>,
                  lower_right: Complex<f64>)
    -> Complex<f64>
{
    let (width, height) = (lower_right.re - upper_left.re,
                           upper_left.im - lower_right.im);
    Complex {
        re: upper_left.re + width  * (pixel.0 as f64/ bounds.0 as f64),
        im: upper_left.im - height * (pixel.1 as f64/ bounds.1 as f64)
    }
}

#[test]
fn test_pixel_to_point() {
    assert_eq!(pixel_to_point((100,100),
                              (50,50),
                              Complex{re:-1.,im:1.},
                              Complex{re:1.,im:-1.}),
               Complex{re:0.,im:0.});
    assert_eq!(pixel_to_point((100,100),
                              (25,75),
                              Complex{re:-1.,im:1.},
                              Complex{re:1.,im:-1.}),
               Complex{re:-0.5,im:-0.5});
}

/// Render a rectangle of the Mandelbrot set into a buffer of pixels.
///
/// The `bounds` argument gives the width and height of the buffer `pixels`,
/// which holds one grayscale pixel per byte. The `upper_left` and `lower_right`
/// arguments specify points on the complex plane corresponding to the upper-
/// left and lower-right corners of the pixel buffer.
fn render(pixels: &mut[u8],
          bounds: (usize, usize),
          upper_left: Complex<f64>,
          lower_right: Complex<f64>)
{
    assert!(pixels.len() == bounds.0 * bounds.1);

    for row in 0..bounds.1 {
        for column in 0..bounds.0 {
            let point = pixel_to_point(bounds, (row, column),
                                       upper_left, lower_right);
            pixels[bounds.0*row+column] =
                match escape_time(point, 255) {
                    None => 0,
                    Some(count) => 255 - count as u8
                }
        }
    }
}

extern crate image;

use image::ColorType;
use image::png::PNGEncoder;
use std::fs::File;

/// Write the buffer `pixels`, whose dimensions are given by
/// `bounds`, to the file named `filename`.
fn write_image(filename: &str, pixels: &[u8], bounds: (usize, usize))
    -> Result<(), std::io::Error>
{
    let mut output = File::create(filename)?;
    let encoder = PNGEncoder::new(output);

    encoder.encode(&pixels,
                   bounds.0 as u32, bounds.1 as u32,
                   ColorType::Gray(8));

    Ok(())
}

use std::io::Write;

fn main() {
    let args: Vec<String> = std::env::args().collect();

    if args.len() != 5 {
        writeln!(std::io::stderr(),
                 "Usage: mandelbrot FILE PIXELS UPPERLEFT LOWERRIGHT")
            .unwrap();
        std::process::exit(1);
    }

    let (bounds, upper_left, lower_right) = (
        parse_pair(&args[2], 'x').expect("error parsing image dimensions"),
        parse_complex(&args[3])  .expect("error parsing upper left corner point"),
        parse_complex(&args[4])  .expect("error parsing lower right corner point")
    );

    let mut pixels = vec![0; bounds.0 * bounds.1];

    render(&mut pixels, bounds, upper_left, lower_right);

    write_image(&args[1], &pixels, bounds)
        .expect("error writing PNG file");
}

