#define R_NO_REMAP
#define STRICT_R_HEADERS

#include <cstring>
#include <string>
#include <vector>

#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <R_ext/Visibility.h>

/* Uncomment to include the action column in the events data.frame. */
#define INCLUDE_ACTION

/**
 * Description of the farm types from Table 1 and Table 2 in Salines
 * M, Andraud M, Rose N, Widgren S (2020) A between-herd data-driven
 * stochastic model to explore the spatio-temporal spread of hepatitis
 * E virus in the French pig production network. PLoS ONE 15(7):
 * e0230257. https://doi.org/10.1371/journal.pone.0230257
 *
 * We have eight farm types: nucleus (NU), multipliers
 * (MU), farrow-to-finish (FF), farrowing (FA), farrowing post-weaning
 * (FPW), post-weaning (PW), post-weaning finishing (PWF) and
 * finishing (FI) farms.
 *
 * Table 1. Types of sectors, animal populations and events per farm
 * depending on the farm type. Farms are composed of one to four
 * sectors, depending on their type. They can rear one to three populations
 * (breeding sows, growing pigs and barrows).
 *
 * Height types of events can occur depending on the farm type:
 * movement of sows from finishing to gestation sector (fi-ges);
 * movement of sows from gestation to farrowing sector (ges-fa);
 * piglet birth (birth);
 * movement of sows from farrowing back to gestation sector (fa-ges);
 * movement of sows from farrowing to slaughter (fa-sl);
 * movement of piglets from farrowing to post-weaning sector (fa-pw);
 * movement of growing pigs from post-weaning to finishing sector (pw-fi);
 * movement of barrows leaving the finishing sector (fi-sl).
 *
 * +------------------------------------+------------------------------------------+-----------+
 * |                                    |                Farm type                 | Slaughter |
 * |                                    +------------------------------------------+-----------+
 * |                                    |  NU | MU | FF | FA | FPW | PW | PWF | FI |     SL    |
 * +--------------------+---------------+-----+----+----+----+-----+----+-----+----+-----------+
 * | Sectors            | Gestation     |  x  |  x |  x |  x |  x  |    |     |    |           |
 * |                    | Farrowing     |  x  |  x |  x |  x |  x  |    |     |    |           |
 * |                    | Post-weaning  |  x  |  x |  x |    |  x  |  x |  x  |    |           |
 * |                    | Finishing     |  x  |  x |  x |    |     |    |  x  |  x |           |
 * |                    | Slaughter     |     |    |    |    |     |    |     |    |     x     |
 * +--------------------+---------------+----------+----+----+-----+----+-----+----+-----------+
 * | Animal populations | Breeding sows |  x  |  x |  x |  x |  X  |    |     |    |           |
 * |                    | Growing pigs  |  x  |  x |  x |    |  x  |  x |  x  |    |           |
 * |                    | Barrows       |  x  |  x |  x |    |     |    |  x  |  x |           |
 * +--------------------+---------------+-----+----+----+----+-----+----+-----+----+-----------+
 * | Events             | ges-fa        |  x  |  x |  x |  x |  x  |    |     |    |           |
 * |                    | birth         |  x  |  x |  x |  x |  x  |    |     |    |           |
 * |                    | fa-ges        |  x  |  x |  x |  x |  x  |    |     |    |           |
 * |                    | fa-sl         |  x  |  x |  x |  x |  x  |    |     |    |           |
 * |                    | fa-pw         |  x  |  x |  x |  x |  x  |    |     |    |           |
 * |                    | pw-fi         |  x  |  x |  x |    |  x  |  x |  x  |    |           |
 * |                    | fi-ges        |  x  |  x |    |    |     |    |     |    |           |
 * |                    | fi-sl         |  x  |  x |  x |    |     |    |  x  |  x |           |
 * +--------------------+---------------+-----+----+----+----+-----+----+-----+----+-----------+
 * based on https://doi.org/10.1371/journal.pone.0230257.t001
 *
 *
 * Table 2. Parameters governing the population dynamics model depend on the batch rearing system.
 *
 * +---------------------------------------------------+-------+-----+-----+-----+--------+-----+
 * |  batch rearing system (number of baches)          |old (7)|  4  |  5  |  7  |  10    |  20 |
 * +---------------------------------------------------+-------+-----+-----+-----+--------+-----+
 * | Duration of a sow reproductive cycle (days)       |   142 |
 * +---------------------------------------------------+-------+-----+-----+-----+--------+-----+
 * |   - Duration in gestating room (days)             |   107 |              114               |
 * |   - Duration in farrowing room (days)             |    35 |  28 |  28 |  35 |  35    |  28 |
 * +---------------------------------------------------+-------+-----+-----+-----+--------+-----+
 * | Duration of a growing pig cycle (days)            |   180 |
 * +---------------------------------------------------+-------+-----+-----+-----+--------+-----+
 * |   - Duration in farrowing room (days)             |    28 |  21 |  21 |  28 |  28    |  21 |
 * |   - Duration in post-weaning room (days)          |    86 |  67 |  53 |  59 |  53    |  46 |
 * |   - Duration in finishing room (days)             |    94 | 101 | 108 | 102 |  94    | 119 |
 * +---------------------------------------------------+-------+-----+-----+-----+--------+-----+
 * | Interval between two successive batches (days)    |    21 |  35 | 28  | 21  |  14/21 | 7   |
 * +---------------------------------------------------+-------+-----+-----+-----+--------+-----+
 * | Annual renewal rate of sow herds (%)              |    40 |              40                |
 * | Renewal rate of sows per batch   (%)              |       |              10                |
 * +---------------------------------------------------+-------+-----+-----+-----+--------+-----+
 * | Number of animals:                                |       |
 * +---------------------------------------------------+-------+-----+-----+-----+--------+-----+
 * |   - Total number of sows in FA and FPW            |   420 |
 * |   - Total number of sows in NU, MU and FF         |   210 |
 * |   - Number of sows per batch in FA and FPW        |    60 |     DEPENDS ON PREMISES
 * |   - Number of sows per batch in NU, MU and FF     |    30 |
 * |   - Number of piglets per litter                  |    12 |
 * |   - Number of piglets per batch in FA and FPW     |   720 |
 * |   - Number of piglets per batch in NU, MU and FF  |   360 |
 * +---------------------------------------------------+-------+-----+-----+-----+--------+-----+
 * based on https://doi.org/10.1371/journal.pone.0230257.t002
 *
 *
 * Table 3. Farms description
 *
 * +----------------------------------+-----------------------------------------------------------+
 * |  farm characteristics            |                     Description                           |
 * +----------------------------------+-----------------------------------------------------------+
 * |   - farm_id                      | farm identification                                       |
 * |   - farm_type                    | farm type                                                 |
 * |   - farm_BRS                     | batch rearing system of the farm                          |
 * +----------------------------------+-----------------------------------------------------------+
 * |  betas                           | transmission rates per sector                             |
 * |   - beta_ges_sector              | transmission rate in gestation sector                     |
 * |   - beta_fa_sector               | transmission rate in farrowing sector                     |
 * |   - beta_pw_sector               | transmission rate in postweaning sector                   |
 * |   - beta_fi_sector               | transmission rate in finishing sector                     |
 * +----------------------------------+-----------------------------------------------------------+
 * |  farm size                       | number of animals in the farm                             |
 * |   - n_sows_farm                  | maximal number of sows in the farm                        |
 * |   - n_pw_farm                    | maximal number of growing pigs in the farm                |
 * |   - n_fi_farm                    | maximal number of barrows in the farm                     |
 * +----------------------------------+-----------------------------------------------------------+
 * |  sector size                     | number of rooms per sector                                |
 * |   - size_ges_sector              | number of rooms in gestation sector                       |
 * |   - size_fa_sector               | number of rooms in farrowing sector                       |
 * |   - size_pw_sector               | number of rooms in postweaning sector                     |
 * |   - size_fi_sector               | number of rooms in finishing sector                       |
 * +----------------------------------+-----------------------------------------------------------+
 * |  room size                       | number of pens per room in a given sector                 |
 * |   - size_ges_room                | number of pens per room in gestation sector               |
 * |   - size_fa_room                 | number of pens per room in farrowing sector               |
 * |   - size_pw_room                 | number of pens per room in postweaning sector             |
 * |   - size_fi_room                 | number of pens per room in finishing sector               |
 * +----------------------------------+-----------------------------------------------------------+
 * |  pen size                        | number of animals per pen in a given sector               |
 * |   - size_ges_pen                 | number of animals per pen in gestation sector             |
 * |   - size_fa_pen                  | number of animals per pen in farrowing sector             |
 * |   - size_pw_pen                  | number of animals per pen in postweaning sector           |
 * |   - size_fi_pen                  | number of animals per pen in finishing sector             |
 * +----------------------------------+-----------------------------------------------------------+
 * |   - interval                     | Interval between two successive batches (days)            |
 * +----------------------------------+-----------------------------------------------------------+
 * |                                  Stages duration                                             |
 * +----------------------------------+-----------------------------------------------------------+
 * |   - duration_ges                 | number of days spent by sows in gestation sector          |
 * |   - duration_pre_birth           | number of days spent by sows alone in farrowing sector    |
 * |   - duration_fa                  | number of days spent by piglets in gestation sector       |
 * |   - duration_pw                  | number of days spent by growing pigs in postweaning sector|
 * |   - duration_fi                  | number of days spent by barrows in finishing sector       |
 * +----------------------------------+-----------------------------------------------------------+
 * |                                 External movements                                           |
 * |                   Trade partners ids and associate probabilities                             |
 * +----------------------------------+-----------------------------------------------------------+
 * |   - topw_id                      | potential receivers of piglets                            |
 * |   - topw_prob                    | associated probabilities                                  |
 * |   - tofi_id                      | potential receivers of growing pigs                       |
 * |   - tofi_prob                    | associated probabilities                                  |
 * |   - fromfi_id                    | potential senders of sows                               |
 * |   - fromfi_prob                  | associated probabilities                                  |
 * |   - tosl_id                      | potential slaughterhouses                                 |
 * |   - tosl_prob                    | associated probabilities                                  |
 * +----------------------------------+-----------------------------------------------------------+
 *
 */

////////////////////////////////////////////////////////////////////////////////
///
/// Forward declarations.
///
////////////////////////////////////////////////////////////////////////////////

class Room;
class Farm;
class Events;

void DoStageGestation(Farm& farm, Room& room, int day, Events& events, int pass);
void DoStagePreBirth(Farm& farm, Room& room, int day, Events& events, int pass);
void DoLeaveStageFarrowing(Farm& farm, Room& room, int day, Events& events, int pass);
void DoStageRenewal(Farm& farm, Room& room, int day, Events& events, int pass);
void DoStagePostWeaning(Farm& farm, Room& room, int day, Events& events, int pass);
void DoStageFinishing(Farm& farm, Room& room, int day, Events& events, int pass);

// Functions to process room actions.
static void
MoveSowsToFarrowing(
    Farm& farm,
    Room& room,
    int day,
    Events& events);

static void
BirthPiglets(
    Farm& farm,
    Room& room,
    int day,
    Events& events);

static void
MoveSowsFromFarrowing(
    Farm& farm,
    Room& room,
    int day,
    Events& events);

static int
MovePigletsToPostWeaningWithinFarm(
    Farm& farm,
    Room& room,
    int day,
    Events& events);

static int
MovePigletsToPostWeaningOtherFarm(
    Farm& farm,
    Room& room,
    int day,
    Events& events);

static void
MovePigsToSlaughter(
    Farm& farm,
    Room& room,
    int day,
    Events& events);

static int
MovePigsToFinishingWithinFarm(
    Farm& farm,
    Room& room,
    int day,
    Events& events);

static int
MovePigsToFinishingOtherFarm(
    Farm& farm,
    Room& room,
    int day,
    Events& events);

static void
MoveBarrowsToGes(
    Farm& farm,
    Room& room,
    int day,
    Events& events);

static void
AddNewSowsToGes(
    Farm& farm,
    Room& room,
    int day,
    Events& events);

void RaiseError(Farm const& farm, int day, std::string msg);

////////////////////////////////////////////////////////////////////////////////
///
/// Enumerations
///
////////////////////////////////////////////////////////////////////////////////

enum FarmType {
    NU,
    MU,
    FF,
    FA,
    FPW,
    PW,
    PWF,
    FI,
    SL
};

enum Stage {
    Empty,
    Cleaning,
    Gestation,
    PreBirth,
    Farrowing,
    Renewal,
    PostWeaning,
    Finishing
};

enum Event {
    EXIT_EVENT,
    ENTER_EVENT,
    INTERNAL_TRANSFER_EVENT,
    EXTERNAL_TRANSFER_EVENT
};

////////////////////////////////////////////////////////////////////////////////
///
/// Utility functions to extract information from R objects.
///
////////////////////////////////////////////////////////////////////////////////

// Get the list element named str from an R list. Returns NULL if a
// list element cannot be found.
static SEXP
get_list_element(SEXP list, std::string const& str)
{
    SEXP names = Rf_getAttrib(list, R_NamesSymbol);

    for (int i = 0; i < Rf_length(list); i++) {
        if(strcmp(CHAR(STRING_ELT(names, i)), str.c_str()) == 0)
            return VECTOR_ELT(list, i);
    }

    return R_NilValue;
}

// Get integer value from list item.
static int
get_integer(SEXP list, std::string const& str, int min_value)
{
    SEXP item = get_list_element(list, str);
    int x = Rf_asInteger(item);
    if (x < min_value)
        Rf_error("Invalid '%s'.", str.c_str());
    return x;
}

// Get double value from list item.
static double
get_double(SEXP list, std::string const& str, double min_value)
{
    SEXP item = get_list_element(list, str);
    double x = Rf_asReal(item);
    if (x < min_value)
        Rf_error("Invalid '%s'.", str.c_str());
    return x;
}

/**
 * Get integer vector from list item.
 *
 * @param list the list that contains named items.
 * @param str name of the list item with integer vector.
 * @param min_value each integer is checked that it is greater or
 *        equal to the min_value.
 * @param size if > 0, then the integer vector must have this size.
 * @return an integer vector.
 */
static std::vector<int>
get_integer_vec(SEXP list, std::string const& str, int min_value, int size)
{
    SEXP item = get_list_element(list, str);
    std::vector<int> x;

    if (size > 0 && size != Rf_length(item)) {
        Rf_error("Invalid '%s'. Length: %i. Expected length: %i.",
                 str.c_str(), Rf_length(item), size);
    }

    for (int i = 0; i < Rf_length(item); i++) {
        if (Rf_isInteger(item)) {
            if (INTEGER(item)[i] == NA_INTEGER || INTEGER(item)[i] < min_value) {
                Rf_error("Invalid '%s'.", str.c_str());
            }
            x.push_back(INTEGER(item)[i]);
        } else if (Rf_isReal(item)) {
            if (!R_FINITE(REAL(item)[i]) || (int)REAL(item)[i] < min_value) {
                Rf_error("Invalid '%s'.", str.c_str());
            }
            x.push_back((int)REAL(item)[i]);
        } else {
            Rf_error("Invalid '%s'.", str.c_str());
        }
    }

    return x;
}

/**
 * Get numeric vector from list item.
 *
 * @param list the list that contains named items.
 * @param str name of the list item with numeric vector.
 * @param min_value each value is checked that it is greater or
 *        equal to the min_value.
 * @param size if > 0, then the numeric vector must have this size.
 * @return an numeric vector.
 */
static std::vector<double>
get_numeric_vec(SEXP list, std::string const& str, double min_value, int size)
{
    SEXP item = get_list_element(list, str);
    std::vector<double> x;

    if (size > 0 && size != Rf_length(item))
        Rf_error("Invalid '%s'.", str.c_str());

    for (int i = 0; i < Rf_length(item); i++) {
        if (Rf_isInteger(item)) {
            if (INTEGER(item)[i] == NA_INTEGER || (double)INTEGER(item)[i] < min_value) {
                Rf_error("Invalid '%s'.", str.c_str());
            }
            x.push_back((double)INTEGER(item)[i]);
        } else if (Rf_isReal(item)) {
            if (!R_FINITE(REAL(item)[i]) || REAL(item)[i] < min_value) {
                Rf_error("Invalid '%s'.", str.c_str());
            }
            x.push_back(REAL(item)[i]);
        } else {
            Rf_error("Invalid '%s'.", str.c_str());
        }
    }

    return x;
}

// Get the farm type from an R list.
static FarmType
get_farm_type(SEXP farm)
{
    SEXP farm_type = get_list_element(farm, "farm_type");

    if (Rf_isNull(farm_type))
        Rf_error("Invalid farm type.");

    if(strcmp(CHAR(STRING_ELT(farm_type, 0)), "NU") == 0)
        return NU;
    if(strcmp(CHAR(STRING_ELT(farm_type, 0)), "MU") == 0)
        return MU;
    if(strcmp(CHAR(STRING_ELT(farm_type, 0)), "FF") == 0)
        return FF;
    if(strcmp(CHAR(STRING_ELT(farm_type, 0)), "FA") == 0)
        return FA;
    if(strcmp(CHAR(STRING_ELT(farm_type, 0)), "FPW") == 0)
        return FPW;
    if(strcmp(CHAR(STRING_ELT(farm_type, 0)), "PW") == 0)
        return PW;
    if(strcmp(CHAR(STRING_ELT(farm_type, 0)), "PWF") == 0)
        return PWF;
    if(strcmp(CHAR(STRING_ELT(farm_type, 0)), "FI") == 0)
        return FI;
    if(strcmp(CHAR(STRING_ELT(farm_type, 0)), "SL") == 0)
        return SL;

    Rf_error("Invalid farm type: '%s'",
             CHAR(STRING_ELT(farm_type, 0)));
}

// Determine the number of days to generate events for.
static int
get_n_days(SEXP days)
{
    int x = Rf_asInteger(days);
    if (x < 1)
        Rf_error("'days' must be an integer > 0.");
    return x;
}

////////////////////////////////////////////////////////////////////////////////
///
/// Code for the 'Events' class.
///
////////////////////////////////////////////////////////////////////////////////

class Events {
public:
    // Add an event to the event list.
    void add(std::string action, int event, int day, int node, int dest, int n, int select, int shift) {
#ifdef INCLUDE_ACTION
        this->action.push_back(action);
#endif
        this->event.push_back(event);
        this->time.push_back(day + 1); // Go from zero-based to one-based time.
        this->node.push_back(node);
        this->dest.push_back(dest);
        this->n.push_back(n);
        this->proportion.push_back(0); // Currently not used, push default value.
        this->select.push_back(select);
        this->shift.push_back(shift);
    }

    // Transform the event list to a data.fram.
    SEXP as_data_frame()  {
        SEXP out, vec;
        int column = 0;

#ifdef INCLUDE_ACTION
        const char *colnames[] = {
            "action", "event", "time", "node", "dest", "n",
            "proportion", "select", "shift", ""};
#else
        const char *colnames[] = {
            "event", "time", "node", "dest", "n",
            "proportion", "select", "shift", ""};
#endif

        // Create a result data.frame and copy the generated data to
        // the columns.
        PROTECT(out = Rf_mkNamed(VECSXP, colnames));
        Rf_setAttrib(out, R_ClassSymbol, Rf_mkString("data.frame"));

#ifdef INCLUDE_ACTION
        SET_VECTOR_ELT(out, column++, vec = Rf_allocVector(STRSXP, action.size()));
        for (std::vector<std::string>::size_type i = 0; i < action.size(); ++i)
            SET_STRING_ELT(vec, i, Rf_mkChar(action[i].c_str()));
#endif

        SET_VECTOR_ELT(out, column++, vec = Rf_allocVector(INTSXP, event.size()));
        memcpy(INTEGER(vec), event.data(), event.size() * sizeof(int));

        SET_VECTOR_ELT(out, column++, vec = Rf_allocVector(INTSXP, time.size()));
        memcpy(INTEGER(vec), time.data(), time.size() * sizeof(int));

        SET_VECTOR_ELT(out, column++, vec = Rf_allocVector(INTSXP, node.size()));
        memcpy(INTEGER(vec), node.data(), node.size() * sizeof(int));

        SET_VECTOR_ELT(out, column++, vec = Rf_allocVector(INTSXP, dest.size()));
        memcpy(INTEGER(vec), dest.data(), dest.size() * sizeof(int));

        SET_VECTOR_ELT(out, column++, vec = Rf_allocVector(INTSXP, n.size()));
        memcpy(INTEGER(vec), n.data(), n.size() * sizeof(int));

        SET_VECTOR_ELT(out, column++, vec = Rf_allocVector(REALSXP, proportion.size()));
        memcpy(REAL(vec), proportion.data(), proportion.size() * sizeof(double));

        SET_VECTOR_ELT(out, column++, vec = Rf_allocVector(INTSXP, select.size()));
        memcpy(INTEGER(vec), select.data(), select.size() * sizeof(int));

        SET_VECTOR_ELT(out, column++, vec = Rf_allocVector(INTSXP, shift.size()));
        memcpy(INTEGER(vec), shift.data(), shift.size() * sizeof(int));

        // Add row names to the 'data.frame'. Note that the row names
        // are one-based.
        Rf_setAttrib(out, R_RowNamesSymbol, vec = Rf_allocVector(INTSXP, event.size()));
        for (std::vector<int>::size_type i = 0; i < event.size(); i++)
            INTEGER(vec)[i] = i + 1;

        UNPROTECT(1);

        return out;
    }

private:
#ifdef INCLUDE_ACTION
    std::vector<std::string> action;
#endif
    std::vector<int> event;
    std::vector<int> time;
    std::vector<int> node;
    std::vector<int> dest;
    std::vector<int> n;
    std::vector<double> proportion;
    std::vector<int> select;
    std::vector<int> shift;
};

////////////////////////////////////////////////////////////////////////////////
///
/// Code for the 'Room' class.
///
////////////////////////////////////////////////////////////////////////////////

class Room {
public:
    /**
     * Initialize the room and pens.
     *
     * @param size_room the number of pens in each room.
     * @param size_pen the maximum number of animals in a pen.
     * @param empty when 'true', zero animals in each pen, else
     *        'size_pen' animals are added to each pen.
     * @param offset the offset to determine the one-based index
     *        (node) of each pen in the sector.
     */
    Room(size_t size_room, size_t size_pen, bool empty, int offset)
        : size_pen(size_pen), offset(offset) {
        for (size_t i = 0; i < size_room; i++) {
            if (empty) {
                pens.push_back(0);
            } else {
                pens.push_back(size_pen);
            }
        }
    }

    // Clear the room and mark the day for when it was emptied.
    void ClearRoom(int day_emptied, int post_duration) {
        if (post_duration > 0)
            stage = Cleaning;
        else
            stage = Empty;

        day = day_emptied + post_duration;
        for (size_t i = 0; i < pens.size(); i++) {
            pens[i] = 0;
        }
    }

    /**
     * Search for the first pen in the room with space for more
     * animals, starting from the supplied pen index.
     *
     * @param pen the index to the first pen in the room to check for
     *        space.
     * @return The index (>= 0) to the pen, or -1 if none is found.
     */
    int FindAvailablePen(size_t pen) {
        while (pen < pens.size()) {
            if (pens[pen] < size_pen)
                break;
            pen++;
        }

        if (pen < pens.size())
            return pen;
        return -1;
    }

    // Determine if the duration for the current stage in the room has
    // reach end-time.
    bool IsCleaningDone(int day) {
        if (stage != Cleaning)
            return false;
        return this->day == day;
    }

    // Determine if the duration for the current stage in the room has
    // reach end-time.
    bool IsStageDurationDone(int day) {
        if (stage == Empty || stage == Cleaning)
            return false;
        return this->day == day;
    }

    // Vector to keep track of the number of animals in each pen in
    // the room.
    std::vector<size_t> pens;

    /**
     * The number of pens in the room.
     */
    size_t size() {
        return pens.size();
    }

    // Maximum number of animals per pen.
    size_t size_pen;

    // The pens are indexed from 0, 1, ..., len-1, i.e. zero-based,
    // within each room. The offset is used to determine the one-based
    // pen-index (a node in SimInf) for a pen among all farms.
    int offset;

    // The current stage of the room in the life-cycle.
    Stage stage;

    // The day when the current stage of the room ends.
    int day;
};

////////////////////////////////////////////////////////////////////////////////
///
/// Code for the 'Sector' class.
///
////////////////////////////////////////////////////////////////////////////////

class Sector {
public:
    /**
     * Initialize the sector with rooms and pens.
     *
     * @param size_sector the number of rooms in the sector.
     * @param size_room the number of pens in each room.
     * @param size_pen the maximum number of animals in a pen.
     * @param empty when 'true', zero animals in each pen, else
     *        'size_pen' animals are added to each pen.
     * @param offset the offset to determine the one-based index
     *        (node) of each pen in the sector.
     */
    void init(int size_sector, int size_room, int size_pen, bool empty, int offset) {
        for (int i = 0; i < size_sector; i++) {
            rooms.push_back(Room(size_room, size_pen, empty, offset));
            offset += size_room;
        }
    }

    /**
     * Initialize the sector with rooms and empty pens.
     *
     * @param farm R-list with data for a farm.
     * @param sector name of the sector, for example, 'ges'.
     * @param offset the offset to determine the one-based index
     *        (node) of each pen in the sector.
     */
    void init(SEXP farm, std::string sector, int offset) {
        int size_sector = get_integer(farm, "size_" + sector + "_sector", 0);
        int size_room = get_integer(farm, "size_" + sector + "_room", 0);
        int size_pen = get_integer(farm, "size_" + sector + "_pen", 0);

        init(size_sector, size_room, size_pen, true, offset);
        for (int i = 0; i < size_sector; ++i) {
            rooms[i].stage = Empty;
            rooms[i].day = 0;
        }
    }

    /**
     * Search for the first pen in the room with space for more
     * animals, starting from the supplied pen index.
     *
     * @param room the index to the room in the sector.
     * @param pen the index to the first pen in the room to check for
     *        space.
     * @return The index (>= 0) to the pen, or -1 if none is found.
     */
    int FindAvailablePen(size_t room, size_t pen) {
        if (room < rooms.size())
            return rooms[room].FindAvailablePen(pen);
        return -1;
    }

    /**
     * Search for an empty room in the sector. If more than one room
     * is empty, returns the room that has been empty for the longest
     * time.
     *
     * @return The index (>= 0) to the room, or -1 if none is found.
     */
    int FindAvailableRoom() {
        int room = -1;

        for (size_t i = 0; i < rooms.size(); ++i) {
            if (rooms[i].stage == Empty) {
                if (room < 0 || rooms[i].day < rooms[room].day) {
                    room = i;
                }
            }
        }

        return room;
    }

    /**
     * Search for a room in the sector that is either empty or has the
     * specified stage and end day. If more than one room is empty,
     * returns the room that has been empty for the longest time.
     *
     * @param stage the stage to find if the room is occupied.
     * @param end_day the last day of the stage that must match the in
     *        the room.
     * @return The index (>= 0) to the room, or -1 if none is found.
     */
    int FindAvailableRoom(Stage stage, int end_day) {
        /* First search for an empty room. */
        int room = FindAvailableRoom();

        if (room < 0) {
            for (size_t i = 0; i < rooms.size(); ++i) {
                if (rooms[i].stage == stage && rooms[i].day == end_day) {
                    room = i;
                }
            }
        }

        return room;
    }

    /**
     * The total number of pens in the sector.
     */
    size_t size() {
        size_t sum = 0;
        for (size_t i = 0; i < rooms.size(); ++i) {
            sum += rooms[i].size();
        }
        return sum;
    }

    // Keep track of the rooms in the sector.
    std::vector<Room> rooms;
};

////////////////////////////////////////////////////////////////////////////////
///
/// Code for the 'Farm' class.
///
////////////////////////////////////////////////////////////////////////////////

// A data structure for a farm.
class Farm {
public:
    Farm(SEXP farm, int offset) {
        id = get_integer(farm, "farm_id", 1);
        renewal = get_double(farm, "renewalrate", 0);

        if (get_farm_type(farm) == SL) {
            sl_sector.init(1, 1, INT_MAX, true, offset);
            return;
        }

        // Initialize the duration in each stage.
        duration_fa = get_integer(farm, "duration_fa", 0);
        duration_pre_birth = get_integer(farm, "duration_pre_birth", 0);
        duration_ges = get_integer(farm, "duration_ges", 0);
        duration_pw = get_integer(farm, "duration_pw", 0);
        duration_fi = get_integer(farm, "duration_fi", 0);

        // Initialize the duration to keep room empty after it's been
        // emptied.
        duration_post_ges = get_integer(farm, "duration_post_ges", 0);
        duration_post_fa = get_integer(farm, "duration_post_fa", 0);
        duration_post_pw = get_integer(farm, "duration_post_pw", 0);
        duration_post_fi = get_integer(farm, "duration_post_fi", 0);

        int size_ges_sector = get_integer(farm, "size_ges_sector", 0);
        int size_ges_room = get_integer(farm, "size_ges_room", 0);
        int size_ges_pen = get_integer(farm, "size_ges_pen", 0);

        // | NU | MU | FF | FA | FPW : Interval is based on Ges_sector size
        std::vector<int> interval;
        switch (get_farm_type(farm)) {
        case NU:
        case MU:
        case FF:
        case FA:
        case FPW:
            interval = get_integer_vec(farm, "interval", 1, size_ges_sector);
            break;

        case PW:
        case PWF:
        case FI:
            break;

        default:
            Rf_error("Unexpected farm type in farm: %i.", id);
        }

        ges_sector.init(size_ges_sector, size_ges_room, size_ges_pen, false, offset);
        offset += ges_sector.size();

        for (int i = 0; i < size_ges_sector; ++i) {
            ges_sector.rooms[i].stage = Gestation;

            if (i > 0) {
                ges_sector.rooms[i].day = ges_sector.rooms[i - 1].day + interval[i];
            } else {
                ges_sector.rooms[i].day = 0;
            }
        }

        fa_sector.init(farm, "fa", offset);
        offset += fa_sector.size();

        pw_sector.init(farm, "pw", offset);
        offset += pw_sector.size();

        fi_sector.init(farm, "fi", offset);
        offset += fi_sector.size();
    }

    void process_cleaning(int day, Events& events) {
        for (size_t i = 0; i < ges_sector.rooms.size(); ++i) {
            if (ges_sector.rooms[i].IsCleaningDone(day)) {
                if (ges_sector.rooms[i].stage == Cleaning)
                    ges_sector.rooms[i].stage = Empty;
            }
        }

        for (size_t i = 0; i < fa_sector.rooms.size(); ++i) {
            if (fa_sector.rooms[i].IsCleaningDone(day)) {
                if (fa_sector.rooms[i].stage == Cleaning)
                    fa_sector.rooms[i].stage = Empty;
            }
        }

        for (size_t i = 0; i < pw_sector.rooms.size(); ++i) {
            if (pw_sector.rooms[i].IsCleaningDone(day)) {
                if (pw_sector.rooms[i].stage == Cleaning)
                    pw_sector.rooms[i].stage = Empty;
            }
        }

        for (size_t i = 0; i < fi_sector.rooms.size(); ++i) {
            if (fi_sector.rooms[i].IsCleaningDone(day)) {
                if (fi_sector.rooms[i].stage == Cleaning)
                    fi_sector.rooms[i].stage = Empty;
            }
        }
    }

    void process_post_weaning(int day, Events& events) {
        for (int pass = 1; pass <= 3; ++pass) {
            for (size_t i = 0; i < pw_sector.rooms.size(); ++i) {
                if (pw_sector.rooms[i].IsStageDurationDone(day)) {
                    switch (pw_sector.rooms[i].stage) {
                    case PostWeaning:
                        DoStagePostWeaning(*this, pw_sector.rooms[i], day, events, pass);
                        break;
                    default:
                        RaiseError(*this, day, "Invalid stage in the pw_sector.");
                        break;
                    }
                }
            }
        }
    }

    void process_farrowing(int day, Events& events) {
        for (int pass = 1; pass <= 3; ++pass) {
            for (size_t i = 0; i < fa_sector.rooms.size(); ++i) {
                if (fa_sector.rooms[i].IsStageDurationDone(day)) {
                    switch (fa_sector.rooms[i].stage) {
                    case PreBirth:
                        DoStagePreBirth(*this, fa_sector.rooms[i], day, events, pass);
                        break;
                    case Farrowing:
                        DoLeaveStageFarrowing(*this, fa_sector.rooms[i], day, events, pass);
                        break;
                    default:
                        RaiseError(*this, day, "Invalid stage in the fa_sector.");
                        break;
                    }
                }
            }
        }
    }

    void process_stages(int day, Events& events, int pass) {
        for (size_t i = 0; i < ges_sector.rooms.size(); ++i) {
            if (ges_sector.rooms[i].IsStageDurationDone(day)) {
                switch (ges_sector.rooms[i].stage) {
                case Gestation:
                    DoStageGestation(*this, ges_sector.rooms[i], day, events, pass);
                    break;
                case Renewal:
                    DoStageRenewal(*this, ges_sector.rooms[i], day, events, pass);
                    break;
                default:
                    RaiseError(*this, day, "Invalid stage in the ges_sector.");
                    break;
                }
            }
        }

        for (size_t i = 0; i < fi_sector.rooms.size(); ++i) {
            if (fi_sector.rooms[i].IsStageDurationDone(day)) {
                switch (fi_sector.rooms[i].stage) {
                case Finishing:
                    DoStageFinishing(*this, fi_sector.rooms[i], day, events, pass);
                    break;
                default:
                    RaiseError(*this, day, "Invalid stage in the fi_sector.");
                    break;
                }
            }
        }
    }

    // Determine the total number of pens in the farm.
    size_t size() {
        return ges_sector.size() +
            fa_sector.size() +
            pw_sector.size() +
            fi_sector.size() +
            sl_sector.size();
    }

    // The farm identifier.
    int id;

    // The renewal rate for sows.
    double renewal;

    // The duration in each stage.
    int duration_fa;
    int duration_pre_birth;
    int duration_ges;
    int duration_pw;
    int duration_fi;
    int duration_post_ges;
    int duration_post_fa;
    int duration_post_pw;
    int duration_post_fi;

    // The farm sectors.
    Sector ges_sector;
    Sector fa_sector;
    Sector pw_sector;
    Sector fi_sector;
    Sector sl_sector;

    // Connections to other farms.
    std::vector<Farm *> topw_farm;
    std::vector<double> topw_prob;
    std::vector<Farm *> tofi_farm;
    std::vector<double> tofi_prob;
    std::vector<Farm *> fromfi_farm;
    std::vector<double> fromfi_prob;
    std::vector<Farm *> tosl_farm;
    std::vector<double> tosl_prob;
};

////////////////////////////////////////////////////////////////////////////////
///
/// Code for sampling a contact 'Farm' when moving animals.
///
////////////////////////////////////////////////////////////////////////////////

class Contact {
public:
    Contact(std::vector<double> &p)
        : prob_sum(0), prob(p) {
        for (size_t i = 0; i < prob.size(); i++) {
            index.push_back(i);
            prob_sum += prob[i];
        }

        /* Sort the probabilities and indices into descending order by
         * the probabilities. */
        revsort(prob.data(), index.data(), prob.size());

        /* Initialize some indices that are used when performing the
         * sampling. */
        i = 0;
        n = prob.size() - 1;
    }

    int sample() {
        int result = -1;

        /* Sample a connected farm to send animals to. Continue to
         * sample farms until there are no more connected farms. */
        if (i < prob.size()) {
            /* Determine the farm index that was sampled. */
            size_t j;
            double cum = 0;
            double rand = prob_sum * unif_rand();
            for (j = 0; j < n; j++) {
                cum += prob[j];
                if (rand <= cum)
                    break;
            }

            result = index[j];
            prob_sum -= prob[j];
            for(size_t k = j; k < n; k++) {
                prob[k] = prob[k + 1];
                index[k] = index[k + 1];
            }

            /* Update the indices. */
            i++;
            n--;
        }

        return result;
    }

private:
    double prob_sum;
    std::vector<double> prob;
    std::vector<int> index;
    size_t i, n;
};

////////////////////////////////////////////////////////////////////////////////
///
/// Functions to process the various stages in the production-cycle.
///
////////////////////////////////////////////////////////////////////////////////

void
DoStageGestation(
    Farm& farm,
    Room& room,
    int day,
    Events& events,
    int pass)
{
    switch (pass) {
    case 1:
        MoveSowsToFarrowing(farm, room, day, events);

        // Clear the room and mark the day for when it was emptied.
        room.ClearRoom(day, farm.duration_post_ges);
        break;

    default:
        Rf_error("DoStageGestation: invalid pass.");
    }
}

void
DoStagePreBirth(
    Farm& farm,
    Room& room,
    int day,
    Events& events,
    int pass)
{
    switch (pass) {
    case 1:
        BirthPiglets(farm, room, day, events);

        // Update the stage in the farrowing room.
        room.stage = Farrowing;
        room.day = day + farm.duration_fa;
        break;

    default:
        Rf_error("DoStagePreBirth: invalid pass.");
    }
}

void
DoLeaveStageFarrowing(
    Farm& farm,
    Room& room,
    int day,
    Events& events,
    int pass)
{
    switch (pass) {
    case 1:
        // The order here is important since we first move one sow and
        // then try to move all remaining animals in the pen.
        MoveSowsFromFarrowing(farm, room, day, events);

        if (MovePigletsToPostWeaningWithinFarm(farm, room, day, events)) {
            // Clear the room and mark the day for when it was
            // emptied.
            room.ClearRoom(day, farm.duration_post_fa);
        }
        break;

    case 2:
        if (MovePigletsToPostWeaningOtherFarm(farm, room, day, events)) {
            // Clear the room and mark the day for when it was
            // emptied.
            room.ClearRoom(day, farm.duration_post_fa);
        }
        break;

    case 3:
        MovePigsToSlaughter(farm, room, day, events);

        // Clear the room and mark the day for when it was emptied.
        room.ClearRoom(day, farm.duration_post_fa);
        break;

    default:
        Rf_error("DoLeaveStageFarrowing: invalid pass.");
    }
}

void
DoStageRenewal(
    Farm& farm,
    Room& room,
    int day,
    Events& events,
    int pass)
{
    switch (pass) {
    case 1:
        // Nothing todo for this pass.
        break;

    case 2:
        MoveBarrowsToGes(farm, room, day, events);
        break;

    case 3:
        // Add sows to fill up the gestation sector.
        AddNewSowsToGes(farm, room, day, events);
        break;

    default:
        Rf_error("DoStageRenewal: invalid pass.");
    }
}

void
DoStagePostWeaning(
    Farm& farm,
    Room& room,
    int day,
    Events& events,
    int pass)
{
    switch (pass) {
    case 1:
        if (MovePigsToFinishingWithinFarm(farm, room, day, events)) {
            // Clear the room and mark the day for when it was emptied.
            room.ClearRoom(day, farm.duration_post_pw);
        }
        break;

    case 2:
        if (MovePigsToFinishingOtherFarm(farm, room, day, events)) {
            // Clear the room and mark the day for when it was emptied.
            room.ClearRoom(day, farm.duration_post_pw);
        }
        break;

    case 3:
        MovePigsToSlaughter(farm, room, day, events);

        // Clear the room and mark the day for when it was emptied.
        room.ClearRoom(day, farm.duration_post_pw);
        break;

    default:
        Rf_error("DoStagePostWeaning: invalid pass.");
    }
}

void
DoStageFinishing(
    Farm& farm,
    Room& room,
    int day,
    Events& events,
    int pass)
{
    switch (pass) {
    case 1:
    case 2:
        // Nothing todo for this pass.
        break;

    case 3:
        MovePigsToSlaughter(farm, room, day, events);

        // Clear the room and mark the day for when it was emptied.
        room.ClearRoom(day, farm.duration_post_fi);
        break;

    default:
        Rf_error("DoStageFinishing: invalid pass.");
    }
}

////////////////////////////////////////////////////////////////////////////////
///
/// Functions to process actions when changing stage in the
/// production-cycle.
///
////////////////////////////////////////////////////////////////////////////////

void
RaiseError(
    Farm const& farm,
    int day,
    std::string msg)
{
    Rf_error("\n"
             "Farm: %i\n"
             "Day:  %i\n"
             "Msg:  %s",
             farm.id,
             day + 1, // Go from zero-based to one-based time.
             msg.c_str());
}

static void
MoveSowsToFarrowing(
    Farm& farm,
    Room& room,
    int day,
    Events& events)
{
    // Iterate over all pens in the room.
    for (size_t pen = 0; pen < room.pens.size(); ++pen) {
        // Find a destination room in the fa_sector for the sows.
        int fa_room = farm.fa_sector.FindAvailableRoom();
        if (fa_room < 0) {
            Rf_error(
                "Unable to find a destination room in the fa_sector. Farm: %i, Day: %i.",
                farm.id,
                day);
        }

        if (farm.fa_sector.rooms[fa_room].pens.size() < room.pens[pen]) {
            Rf_error(
                "Destination room in the fa_sector is to small. Farm: %i, Day: %i.",
                farm.id,
                day);
        }

        // Iterate over all sows in the pen.
        for (size_t i = 0; i < room.pens[pen]; ++i) {
            // Add the sow in the fa_sector.
            farm.fa_sector.rooms[fa_room].pens[i] = 1;

            // Record the movement event.
            events.add(
                "MoveSowsToFarrowing",
                EXTERNAL_TRANSFER_EVENT,
                day,
                pen + room.offset,
                i + farm.fa_sector.rooms[fa_room].offset,
                1,
                5,
                0);
        }

        // Update the stage in the fa_room.
        farm.fa_sector.rooms[fa_room].stage = PreBirth;
        farm.fa_sector.rooms[fa_room].day = day + farm.duration_pre_birth;
    }
}

static void
BirthPiglets(
    Farm& farm,
    Room& room,
    int day,
    Events& events)
{
    // Iterate over all pens in the farrowing room.
    for (size_t pen = 0; pen < room.pens.size(); ++pen) {
        if (room.pens[pen] < room.size_pen) {
            // Add piglets to the pen.
            size_t n = room.size_pen - room.pens[pen];
            room.pens[pen] += n;

            // Record the enter event.
            events.add(
                "BirthPiglets",
                ENTER_EVENT,
                day,
                pen + room.offset,
                0,
                n,
                7,
                0);
        }
    }
}

static void
MoveSowsFromFarrowing(
    Farm& farm,
    Room& room,
    int day,
    Events& events)
{
    // Find a destination room in the ges_sector for the sows.
    int ges_room = farm.ges_sector.FindAvailableRoom();
    if (ges_room < 0) {
        RaiseError(farm, day, "Unable to find a destination room in the ges_sector.");
    }

    // Check that we have a pen in the gestation room.
    if (farm.ges_sector.rooms[ges_room].pens.size() < 1) {
        RaiseError(farm, day, "Destination room in the ges_sector is to small.");
    }

    /* Determine if the farm is connected to a slaughter node. */
    int event = EXIT_EVENT;
    int dest = 0;
    if (!farm.tosl_prob.empty()) {
        Contact d(farm.tosl_prob);
        int i = d.sample();
        if (i >= 0) {
            dest = farm.tosl_farm[i]->sl_sector.rooms[0].offset;
            event = EXTERNAL_TRANSFER_EVENT;
        }
    }

    // Iterate over all pens in the room.
    Stage next_stage = Gestation;
    for (size_t pen = 0; pen < room.pens.size(); ++pen) {
        if (room.pens[pen] > 0) {
            // Remove the sow from the pen.
            room.pens[pen] -= 1;

            if (unif_rand() <= farm.renewal) {
                next_stage = Renewal;

                // Move sows to slaughter. Either use an exit event or
                // move to a slaughter node instead so that we can
                // measure the prevalence.
                events.add(
                    "MoveSowsToSlaughter",
                    event,
                    day,
                    pen + room.offset,
                    dest,
                    1,
                    5,
                    0);
            } else {
                // Add the sow to the pen.
                farm.ges_sector.rooms[ges_room].pens[0] += 1;

                // Record the movement event.
                events.add(
                    "MoveSowsFromFarrowing",
                    EXTERNAL_TRANSFER_EVENT,
                    day,
                    pen + room.offset,
                    farm.ges_sector.rooms[ges_room].offset,
                    1,
                    5,
                    0);
            }
        }
    }

    // Update the stage in the ges_room.
    farm.ges_sector.rooms[ges_room].stage = next_stage;
    farm.ges_sector.rooms[ges_room].day = day;
    if (next_stage == Gestation)
        farm.ges_sector.rooms[ges_room].day += farm.duration_ges;
}

/**
 * Move piglets to the post-weaning sector within the same farm.
 *
 * @param farm the farm.
 * @param room the room in the farm.
 * @param day the current day.
 * @param events the storage for the generated events.
 * @return 1 if all piglets were moved to a post-weaning room within
 *         the farm, else 0 (then one or more passes must be
 *         performed).
 */
static int
MovePigletsToPostWeaningWithinFarm(
    Farm& farm,
    Room& room,
    int day,
    Events& events)
{
    // Find a destination room in the pw_sector for the piglets.
    int pw_room = farm.pw_sector.FindAvailableRoom();
    if (pw_room < 0) {
        return 0;
    }

    // Update the stage in the pw_room.
    farm.pw_sector.rooms[pw_room].stage = PostWeaning;
    farm.pw_sector.rooms[pw_room].day = day + farm.duration_pw;

    // Iterate over all pens in the room.
    int pw_pen = 0;
    for (size_t pen = 0; pen < room.pens.size(); ++pen) {
        // Move all animals in the pen.
        while (room.pens[pen] > 0) {
            // Find a pen in the pw_sector with space for more
            // piglets.
            pw_pen = farm.pw_sector.FindAvailablePen(pw_room, pw_pen);
            if (pw_pen < 0) {
                return 0;
            }

            // Determine how many animals that can be moved.
            size_t space = farm.pw_sector.rooms[pw_room].size_pen -
                farm.pw_sector.rooms[pw_room].pens[pw_pen];
            size_t n = std::min(space, room.pens[pen]);

            // Update the pens.
            room.pens[pen] -= n;
            farm.pw_sector.rooms[pw_room].pens[pw_pen] += n;

            // Record the movement events.
            events.add(
                "MovePigletsToPostWeaningWithinFarm",
                EXTERNAL_TRANSFER_EVENT,
                day,
                pen + room.offset,
                pw_pen + farm.pw_sector.rooms[pw_room].offset,
                n,
                12,
                0);
        }
    }

    return 1;
}

/**
 * Move piglets to the post-weaning sector in another farm.
 *
 * @param farm the farm.
 * @param room the room in the farm.
 * @param day the current day.
 * @param events the storage for the generated events.
 * @return 1 if all piglets were moved to a post-weaning room in
 *         another farm, else 0 (then one more pass must be
 *         performed).
 */
static int
MovePigletsToPostWeaningOtherFarm(
    Farm& farm,
    Room& room,
    int day,
    Events& events)
{
    /* Check if there any connected farms. */
    if (farm.topw_prob.empty()) {
        return 0;
    }

    Contact dest(farm.topw_prob);
    size_t pen = 0;
    int i;
    while ((i = dest.sample()) >= 0) {
        int pw_pen = 0;

        // Find a destination room in the pw_sector for the piglets.
        int pw_room = farm.topw_farm[i]->pw_sector.FindAvailableRoom(PostWeaning, day + farm.duration_pw);
        if (pw_room < 0) {
            continue;
        }

        // Update the stage in the pw_room.
        farm.topw_farm[i]->pw_sector.rooms[pw_room].stage = PostWeaning;
        farm.topw_farm[i]->pw_sector.rooms[pw_room].day = day + farm.duration_pw;

        // Iterate over all pens in the room.
        while (pen < room.pens.size()) {
            // Move all animals in the pen.
            while (room.pens[pen] > 0) {
                // Find a pen in the pw_sector with space for more piglets.
                pw_pen = farm.topw_farm[i]->pw_sector.FindAvailablePen(pw_room, pw_pen);
                if (pw_pen < 0) {
                    break;
                }

                // Determine how many animals that can be moved.
                size_t space = farm.topw_farm[i]->pw_sector.rooms[pw_room].size_pen -
                    farm.topw_farm[i]->pw_sector.rooms[pw_room].pens[pw_pen];
                size_t n = std::min(space, room.pens[pen]);

                // Update the pens.
                room.pens[pen] -= n;
                farm.topw_farm[i]->pw_sector.rooms[pw_room].pens[pw_pen] += n;

                // Record the movement events.
                events.add(
                    "MovePigletsToPostWeaningOtherFarm",
                    EXTERNAL_TRANSFER_EVENT,
                    day,
                    pen + room.offset,
                    pw_pen + farm.topw_farm[i]->pw_sector.rooms[pw_room].offset,
                    n,
                    12,
                    0);
            }

            if (pw_pen < 0)
                break;
            pen++;
        }
    }

    for (pen = 0; pen < room.pens.size(); pen++) {
        if (room.pens[pen] > 0) {
            return 1;
        }
    }

    return 0;
}

/**
 * Move pigs from a room to slaughter. This is a pass=3 step if there
 * are no space within or in connected farms.
 *
 * @param farm the farm.
 * @param room the room in the farm.
 * @param day the current day.
 * @param events the storage for the generated events.
 */
static void
MovePigsToSlaughter(
    Farm& farm,
    Room& room,
    int day,
    Events& events)
{
    int event = EXIT_EVENT;
    int dest = 0;

    /* Determine if the farm is connected to a slaughter node. */
    if (!farm.tosl_prob.empty()) {
        Contact d(farm.tosl_prob);
        int i = d.sample();
        if (i >= 0) {
            dest = farm.tosl_farm[i]->sl_sector.rooms[0].offset;
            event = EXTERNAL_TRANSFER_EVENT;
        }
    }

    /* Move all animals in each pen to slaughter. Either use an exit
     * event or move to a slaughter node instead so that we can
     * measure the prevalence. */
    for (size_t pen = 0; pen < room.pens.size(); ++pen) {
        if (room.pens[pen] > 0) {
            events.add(
                "MovePigsToSlaughter",
                event,
                day,
                pen + room.offset,
                dest,
                room.pens[pen],
                12,
                0);

            room.pens[pen] = 0;
        }
    }
}

static int
MovePigsToFinishingWithinFarm(
    Farm& farm,
    Room& room,
    int day,
    Events& events)
{
    /* Find a destination room in the fi_sector. */
    int fi_room = farm.fi_sector.FindAvailableRoom();
    if (fi_room < 0) {
        return 0;
    }

    /* Iterate over all pens in the room. */
    int fi_pen = 0;
    for (size_t pen = 0; pen < room.pens.size(); ++pen) {
        /* Move all animals in the pen. */
        while (room.pens[pen] > 0) {
            /* Find a pen in the fi_sector with space for more
             * animals. */
            fi_pen = farm.fi_sector.FindAvailablePen(fi_room, fi_pen);
            if (fi_pen < 0) {
                return 0;
            }

            /* Determine how many animals that can be moved. */
            size_t space = farm.fi_sector.rooms[fi_room].size_pen -
                farm.fi_sector.rooms[fi_room].pens[fi_pen];
            size_t n = std::min(space, room.pens[pen]);

            /* Update the pens. */
            room.pens[pen] -= n;
            farm.fi_sector.rooms[fi_room].pens[fi_pen] += n;

            /* Record the movement event. */
            events.add(
                "MovePigsToFinishingWithinFarm",
                EXTERNAL_TRANSFER_EVENT,
                day,
                pen + room.offset,
                fi_pen + farm.fi_sector.rooms[fi_room].offset,
                n,
                12,
                0);
        }
    }

    /* Update the stage in the fi_room. */
    farm.fi_sector.rooms[fi_room].stage = Finishing;
    farm.fi_sector.rooms[fi_room].day = day + farm.duration_fi;

    return 1;
}

static int
MovePigsToFinishingOtherFarm(
    Farm& farm,
    Room& room,
    int day,
    Events& events)
{
    /* Check if there any connected farms. */
    if (farm.tofi_prob.empty()) {
        return 0;
    }

    Contact dest(farm.tofi_prob);
    size_t pen = 0;
    int i;
    while ((i = dest.sample()) >= 0) {
        int fi_pen = 0;

        // Find a pen in the fi_sector with space for more animals.
        int fi_room = farm.tofi_farm[i]->fi_sector.FindAvailableRoom(
            Finishing, day + farm.duration_fi);
        if (fi_room < 0) {
            continue;
        }

        // Update the stage in the fi_room.
        farm.tofi_farm[i]->fi_sector.rooms[fi_room].stage = Finishing;
        farm.tofi_farm[i]->fi_sector.rooms[fi_room].day = day + farm.duration_fi;

        // Iterate over all pens in the room.
        while (pen < room.pens.size()) {
            // Move all animals in the pen.
            while (room.pens[pen] > 0) {
                // Find a pen in the fi_sector with space for more animals.
                fi_pen = farm.tofi_farm[i]->fi_sector.FindAvailablePen(fi_room, fi_pen);
                if (fi_pen < 0) {
                    break;
                }

                // Determine how many animals that can be moved.
                size_t space = farm.tofi_farm[i]->fi_sector.rooms[fi_room].size_pen -
                    farm.tofi_farm[i]->fi_sector.rooms[fi_room].pens[fi_pen];
                size_t n = std::min(space, room.pens[pen]);

                // Update the pens.
                room.pens[pen] -= n;
                farm.tofi_farm[i]->fi_sector.rooms[fi_room].pens[fi_pen] += n;

                // Record the movement events.
                events.add(
                    "MovePigsToFinishingOtherFarm",
                    EXTERNAL_TRANSFER_EVENT,
                    day,
                    pen + room.offset,
                    fi_pen + farm.tofi_farm[i]->fi_sector.rooms[fi_room].offset,
                    n,
                    12,
                    0);
            }

            if (fi_pen < 0)
                break;
            pen++;
        }
    }

    for (pen = 0; pen < room.pens.size(); pen++) {
        if (room.pens[pen] > 0) {
            return 1;
        }
    }

    return 0;
}

static void
AddNewSowsToGes(
    Farm& farm,
    Room& room,
    int day,
    Events& events)
{
    // Iterate over all pens in the gestation room and make sure
    // that they are all full.
    for (size_t pen = 0; pen < room.pens.size(); ++pen) {
        // Check if the pen in the room needs more animals.
        while (room.pens[pen] < room.size_pen) {
            size_t n = room.size_pen - room.pens[pen];

            // This can, for example, happen in the initial phase
            // before any animals have been moved to the finishing
            // sector. Add animals to fill the pen.

            // Update the pen and record the enter event.
            room.pens[pen] += n;
            events.add(
                "AddNewSowsToGes",
                ENTER_EVENT,
                day,
                pen + room.offset,
                0,
                n,
                1,
                0);
        }
    }

    room.stage = Gestation;
    room.day = day + farm.duration_ges;
}

static void
MoveBarrowsToGes(
    Farm& farm,
    Room& room,
    int day,
    Events& events)
{
    /* Check if there any connected farms. */
    if (farm.fromfi_prob.empty())
        return;

    Contact from(farm.fromfi_prob);
    int i;
    while ((i = from.sample()) >= 0) {
        int fi_room = -1;

        /* Try to find a finishing room in a connected farm. */
        for (size_t j = 0; j < farm.fromfi_farm[i]->fi_sector.rooms.size(); ++j) {
            if (farm.fromfi_farm[i]->fi_sector.rooms[j].stage == Finishing) {
                if (fi_room < 0 ||
                    farm.fromfi_farm[i]->fi_sector.rooms[j].day < farm.fromfi_farm[i]->fi_sector.rooms[fi_room].day)
                {
                    fi_room = j;
                }
            }
        }

        if (fi_room >= 0) {
            size_t pen = 0, fi_pen = 0;

            /* Iterate over all pens in the gestation room and move
             * animals from the fi_room in the connected farm. */
            while (pen < room.pens.size() &&
                   fi_pen < farm.fromfi_farm[i]->fi_sector.rooms[fi_room].pens.size())
            {
                size_t n = room.size_pen - room.pens[pen];
                n = std::min(n, farm.fromfi_farm[i]->fi_sector.rooms[fi_room].pens[fi_pen]);

                if (n > 0) {
                    /* Update the pens. */
                    room.pens[pen] += n;
                    farm.fromfi_farm[i]->fi_sector.rooms[fi_room].pens[fi_pen] -= n;

                    /* Record the movement event. Use shift = 1 so
                     * that pigs become sows. */
                    events.add(
                        farm.id == farm.fromfi_farm[i]->id ? "MoveBarrowsToGesWithinFarm" : "MoveBarrowsToGesOtherFarm",
                        EXTERNAL_TRANSFER_EVENT,
                        day,
                        fi_pen + farm.fromfi_farm[i]->fi_sector.rooms[fi_room].offset,
                        pen + room.offset,
                        n,
                        12,
                        1);
                }

                if (room.pens[pen] >= room.size_pen) {
                    pen++;

                    if (pen >= room.pens.size()) {
                        room.stage = Gestation;
                        room.day = day + farm.duration_ges;
                        return;
                    }
                }

                if (farm.fromfi_farm[i]->fi_sector.rooms[fi_room].pens[fi_pen] == 0)
                    fi_pen++;
            }
        }
    }
}

////////////////////////////////////////////////////////////////////////////////
///
/// Main C++ function to be called from R to generate the intra- and
/// inter-herd events.
///
////////////////////////////////////////////////////////////////////////////////

extern "C" SEXP
HEVevents(SEXP farm_list, SEXP days)
{
    Events events;
    std::vector<Farm> farms;
    const int n_days = get_n_days(days);

    if (!Rf_isNewList(farm_list)) {
        Rf_error("Invalid farm list.");
    }

    // Generate a farm list.
    for (int i = 0, offset = 1; i < Rf_length(farm_list); i++) {
        farms.push_back(Farm(VECTOR_ELT(farm_list, i), offset));

        if (farms.back().id != (i + 1))
            Rf_error("Invalid farm id.");

        offset += farms.back().size();
    }

    // Connect each farm to any post-weaning farms.
    for (int i = 0; i < Rf_length(farm_list); i++) {
        std::vector<int> topw_id =
            get_integer_vec(VECTOR_ELT(farm_list, i), "topw_id", 1, 0);

        if (!topw_id.empty()) {
            farms[i].topw_prob =
                get_numeric_vec(VECTOR_ELT(farm_list, i), "topw_prob", 0, topw_id.size());

            for (size_t j = 0; j < topw_id.size(); j++) {
                if (topw_id[j] == farms[i].id)
                    Rf_error("Self-loop identified in 'topw_id' for farm: %i.", farms[i].id);
                if ((size_t)topw_id[j] > farms.size())
                    Rf_error("Invalid 'topw_id': %i detected in farm: %i.", topw_id[j], farms[i].id);

                farms[i].topw_farm.push_back(&farms[topw_id[j] - 1]);
            }
        }
    }

    // Connect each farm to any finishing farms.
    for (int i = 0; i < Rf_length(farm_list); i++) {
        std::vector<int> tofi_id =
            get_integer_vec(VECTOR_ELT(farm_list, i), "tofi_id", 1, 0);

        if (!tofi_id.empty()) {
            farms[i].tofi_prob =
                get_numeric_vec(VECTOR_ELT(farm_list, i), "tofi_prob", 0, tofi_id.size());

            for (size_t j = 0; j < tofi_id.size(); j++) {
                if (tofi_id[j] == farms[i].id)
                    Rf_error("Self-loop identified in 'tofi_id' for farm: %i.", farms[i].id);
                if ((size_t)tofi_id[j] > farms.size())
                    Rf_error("Invalid 'tofi_id' detected in farm: %i.", farms[i].id);

                farms[i].tofi_farm.push_back(&farms[tofi_id[j] - 1]);
            }
        }
    }

    // Connect renewal of sows from other farms.
    for (int i = 0; i < Rf_length(farm_list); i++) {
        std::vector<int> fromfi_id =
            get_integer_vec(VECTOR_ELT(farm_list, i), "fromfi_id", 1, 0);

        if (!fromfi_id.empty()) {
            farms[i].fromfi_prob =
                get_numeric_vec(VECTOR_ELT(farm_list, i), "fromfi_prob", 0, fromfi_id.size());

            for (size_t j = 0; j < fromfi_id.size(); j++) {
                if ((size_t)fromfi_id[j] > farms.size())
                    Rf_error("Invalid 'fromfi_id' detected in farm: %i.", farms[i].id);

                farms[i].fromfi_farm.push_back(&farms[fromfi_id[j] - 1]);
            }
        }
    }

    // Connect each farm to any slaughter.
    for (int i = 0; i < Rf_length(farm_list); i++) {
        std::vector<int> tosl_id =
            get_integer_vec(VECTOR_ELT(farm_list, i), "tosl_id", 1, 0);

        if (!tosl_id.empty()) {
            farms[i].tosl_prob =
                get_numeric_vec(VECTOR_ELT(farm_list, i), "tosl_prob", 0, tosl_id.size());

            for (size_t j = 0; j < tosl_id.size(); j++) {
                if (tosl_id[j] == farms[i].id)
                    Rf_error("Self-loop identified in 'tosl_id' for farm: %i.", farms[i].id);
                if ((size_t)tosl_id[j] > farms.size())
                    Rf_error("Invalid 'tosl_id' detected in farm: %i.", farms[i].id);

                farms[i].tosl_farm.push_back(&farms[tosl_id[j] - 1]);
            }
        }
    }

    // Setup random number generator.
    GetRNGstate();

    // Iterate over the days to generate events for the farms.
    for (int day = 0; day < n_days; day++) {
        // Process cleaning in all farms.
        for (size_t farm = 0; farm < farms.size(); ++farm) {
            farms[farm].process_cleaning(day, events);
        }

        // Process post-weaning in all farms.
        for (size_t farm = 0; farm < farms.size(); ++farm) {
            farms[farm].process_post_weaning(day, events);
        }

        // Process post-weaning in all farms.
        for (size_t farm = 0; farm < farms.size(); ++farm) {
            farms[farm].process_farrowing(day, events);
        }

        for (int pass = 1; pass <= 3; ++pass) {
            // Iterate over all farms.
            for (size_t farm = 0; farm < farms.size(); ++farm) {
                farms[farm].process_stages(day, events, pass);
            }
        }
    }

    PutRNGstate();

    return events.as_data_frame();
}

////////////////////////////////////////////////////////////////////////////////
///
/// Register C++ routines to be called from R.
///
////////////////////////////////////////////////////////////////////////////////

static const R_CallMethodDef callMethods[] =
{
    {"HEVevents", (DL_FUNC) &HEVevents, 2},
    {NULL, NULL, 0}
};

void attribute_visible
R_init_HEVevents(DllInfo *info)
{
    R_registerRoutines(info, NULL, callMethods, NULL, NULL);
    R_useDynamicSymbols(info, FALSE);
    R_forceSymbols(info, TRUE);
}
