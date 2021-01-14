#include "extract_fails.hpp"
#include "grade_criteria.hpp"
#include <algorithm>

/* algorithm act on container elements, instead of containers.*/
/* operations that act on containers are member func of containers
 * themselves */

using std::vector;

// two pass way.
vector<Student_info> extract_fails(vector<Student_info>& students) {
    vector<Student_info> fail;
    std::remove_copy_if( students.begin(), students.end()
                       , std::back_inserter(fail), pgrade);

    students.erase( std::remove_if( students.begin()
                                  , students.end()
                                  , fgrade)
                  , students.end());

    return fail;
}

// one pass way.
vector<Student_info> extract_fails_part(vector<Student_info>& students) {
    vector<Student_info>::iterator iter =
        std::stable_partition(students.begin(), students.end(), pgrade);
    vector<Student_info> fail(iter, students.end());
    return fail;
}
