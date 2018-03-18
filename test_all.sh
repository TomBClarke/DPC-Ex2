#!/bin/bash

# --- Constant definitions. ---

# x is empty.
x=''
# The name of this script.
scriptName='test_all.sh'

# --- --- ---


# --- Function definitions. ---

function error_exit
{
#   ----------------------------------------------------------------
#   Print error message and exit.
#       Accepts 2 argument:
#           string - optional - error message
#           integer - optional - the exit code
#   ----------------------------------------------------------------
    echo "${scriptName}: ${1:-"Unknown Error"}" 1>&2
    exit ${2:-"1"}
}

function print_output_and_exit
{
    echo 'The output was:' >&2
    echo '' >&2
    echo "${1}"
    exit 1
}

function bad_good
{
    echo "Example data file '${1}' exited badly." >&2
    echo "The exit status was: ${3}"
    print_output_and_exit "${2}"
}

function bad_output
{
    echo "Example data file '${1}' did not produce a valid output." >&2
    print_output_and_exit "${2}"
}

function good_bad
{
    echo "Bad input file '${1}' exited without error." >&2
    print_output_and_exit "${2}"
}

function bad_error
{
    echo "Bad input file '${1}' didn't output a custom error message." >&2
    print_output_and_exit "${2}"
}

function check_good_output
{
    if ! [[ "${1}" =~ ^([A-Za-z0-9]+)(\ ([A-Za-z0-9]+))*$ ]]; then
        return 1
    fi
    return 0
}

function check_custom_error
{
    local good_start='{"init terminating in do_boot",{"'
    if [ "${1:0:${#good_start}}" != "${good_start}" ]; then
        return 1
    fi
    return 0
}

# --- --- ---


# --- Determine the location of this script. ---

if [ ! -z "${0+x}" ]\
&& [ -e "${0}" ]\
&& [[ "${0}" == *"${scriptName}" ]]\
&& [ "$( basename "${0}" )" = "${scriptName}" ]; then
    scriptLocation="$( dirname "${0}" )"
elif [ -e "$( pwd )/${scriptName}" ]; then
    scriptLocation="$( pwd )"
elif [ -e "$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )/${scriptName}" ]; then
    scriptLocation="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
else
    error_exit 'Can not determine the location of this script...\nPlease run this script from the same folder as the script.' 26
fi

# --- --- ---


# --- Run the scripts. ---

for good in "${scriptLocation}/example_data/"*'.txt'; do
    output="$( bash "${scriptLocation}/run.sh" "${good}" 2>&1 )"
    status="${?}"
    if [[ "${status}" != 0 ]]; then
        bad_good "${good}" "${output}" "${status}"
    fi
    check_good_output "${output}"
    goodOutput="${?}"
    if [[ "${goodOutput}" != 0 ]]; then
        bad_output "${good}" "${output}"
    fi
done

for bad in "${scriptLocation}/bad_inputs/"*'.txt'; do
    output="$( bash "${scriptLocation}/run.sh" "${bad}" 2>&1 )"
    status="${?}"
    if [[ "${status}" == 0 ]]; then
        good_bad "${bad}" "${output}"
    fi
    check_custom_error "${output}"
    customError="${?}"
    if [[ "${customError}" != 0 ]]; then
        bad_error "${bad}" "${output}"
    fi
done

echo 'All tests passed!'

# --- --- ---
