#include "grade_criteria.hpp"
#include "grade.hpp"

bool fgrade(const Student_info& student) {
    return grade(student) < 60;
}

bool pgrade(const Student_info& student) {
    return !fgrade(student);
}
