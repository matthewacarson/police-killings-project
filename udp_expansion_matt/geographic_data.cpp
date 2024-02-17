#include <iostream>
#include <fstream>
#include <boost/geometry.hpp>
#include <boost/geometry/geometries/point.hpp>
#include <boost/geometry/io/wkt/write.hpp>
#include <boost/geometry/geometries/register/point.hpp>
#include <boost/geometry/geometries/register/multi_point.hpp>
#include <boost/geometry/geometries/register/linestring.hpp>
#include <boost/geometry/io/io.hpp>
#include <boost/geometry/io/wkt/wkt.hpp>

namespace bg = boost::geometry;

// Register your point type to enable Boost.Geometry IO
BOOST_GEOMETRY_REGISTER_POINT_2D(
    boost::geometry::model::point<double, 2, boost::geometry::cs::cartesian>,
    double,
    boost::geometry::cs::cartesian,
    x,
    y
);

BOOST_GEOMETRY_REGISTER_MULTI_POINT(std::vector<bg::model::point<double, 2, bg::cs::cartesian>>);

int main() {
    typedef bg::model::point<double, 2, bg::cs::cartesian> point_type;
    typedef bg::model::multi_point<point_type> multi_point_type;

    // Example data
    std::vector<double> X = {1.0, 2.0, 3.0};
    std::vector<double> Y = {4.0, 5.0, 6.0};

    // Create a collection of points
    multi_point_type points;
    for (size_t i = 0; i < X.size(); ++i) {
        bg::append(points, point_type(X[i], Y[i]));
    }

    // Save points to a shapefile
    std::ofstream output("points.shp");
    bg::write(output, points);
    output.close();

    return 0;
}
