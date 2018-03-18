#!/bin/bash

# --- Constant definitions. ---

# x is empty.
x=''
# The name of this script.
scriptName='run.sh'

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

# --- --- ---


# --- Check arguments. ---

if [ -z "${1+x}" ]; then
    error_exit "Usage: ./${scriptName} input_data_file" 25
fi

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

fileContent="$( cat "${1}" )"
cd "${scriptLocation}"
echo "${fileContent}" | erl -noshell -run tarry main -run init stop

# --- --- ---
