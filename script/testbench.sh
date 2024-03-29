#!/usr/bin/env bash

# This script is used to run testbench.

# no pipefail because sysyc itself may crash :)
set +e

# project root
kirara_root=${KIRARA_ROOT:-"$(dirname $(realpath $0))/.."}
# runtime sysroot
kirara_sysroot=${KIRARA_SYSROOT:-"/"}
# testbench root
testbench_root=${KIRARA_BENCH:-"$kirara_root/testbench"}
# location of kirara compiler
sysyc=${KIRARA_SYSYC:-"target/debug/compiler"}
# location of (riscv) compiler (for assembly and linking)
_host=$(uname -m)
case $_host in
    riscv64)
        _default_cc=${KIRARA_CC:-"gcc"}
        ;;
    *)
        _default_cc=${KIRARA_CC:-"riscv64-linux-gnu-gcc"}
        ;;
esac
cc=${KIRARA_CC:-$_default_cc}
# extra flags
cflags=${KIRARA_CFLAGS:-"-L$testbench_root -lsysy"}
# qemu config
case $_host in
    riscv64)
        qemu=
        qemu_flags=
        ;;
    *)
        qemu=${KIRARA_QEMU:-"qemu-riscv64-static"}
        qemu_flags=${KIRARA_QEMU_FLAGS:-"-cpu rv64,zba=true,zbb=true"}
        ;;
esac
# enabled benchmarks
benchmarks=(
    # debug
    # functional
    # hidden_functional
    performance
    # hidden_performance
)

function clean() {
    # Clean output directory
    for benchmark in ${benchmarks[@]}; do
        rm -rf $testbench_root/$benchmark.asm/*
        rm -rf $testbench_root/$benchmark.out/*
        rm -rf $testbench_root/$benchmark.run/*
    done
}

function make_output_dirs() {
    # Make output directory
    for benchmark in ${benchmarks[@]}; do
        mkdir -p $testbench_root/$benchmark.asm
        mkdir -p $testbench_root/$benchmark.out
        mkdir -p $testbench_root/$benchmark.run
    done
}

function expand_short_case_name() {
    _suite=$1
    _case=$2
    # expand case name if it is shorter than 5 characters
    _len=${#_case}
    if [[ $len -lt 5 ]]; then
        # expand case number with its name by searching the files starting with the number
        _case=$(basename -s .sy $(find $testbench_root/$_suite -name "$_case*.sy"))
    fi
    echo $_case
}

function compile_test_case() {
    _suite=$1
    _case=$(expand_short_case_name $@)
    shift 2
    _sysyflags=$@
    _source=$testbench_root/$_suite/$_case.sy
    _asm=$testbench_root/$_suite.asm/$_case.S
    _out=$testbench_root/$_suite.out/$_case.out
    
    echo "Compiling $_suite::$_case"
    $sysyc $_source -o $_asm $_sysyflags
    if [ $? -ne 0 ]; then
        echo "Failed to compile $_suite::$_case"
        exit 1
    fi
    $cc $_asm -o $_out $cflags
    if [ $? -ne 0 ]; then
        echo "Failed to assemble $_suite::$_case"
        exit 1
    fi
    echo "Done"
}

function file_ends_with_newline() {
    [[ $(tail -c1 "$1" | wc -l) -gt 0 ]]
}

function run_test_case() {
    _suite=$1
    _case=$(expand_short_case_name $@)
    
    _out=$testbench_root/$_suite.out/$_case.out
    _input=$testbench_root/$_suite/$_case.in
    _ref=$testbench_root/$_suite/${_case/.sy/}.out
    _output=$testbench_root/$_suite.run/$_case.log

    echo "Running $_suite::$_case"
    export QEMU_LD_PREFIX=$kirara_sysroot
    # export QEMU_STACK_SIZE=100000000
    # input is optional
    if [ ! -f $_input ]; then
        $qemu $qemu_flags $_out > $_output
    else
        $qemu $qemu_flags $_out < $_input > $_output
    fi
    # read from _output
    _exit_code=$?
    if ! file_ends_with_newline $_output; then
        if [[ ! -z $(cat $_output) ]]; then
            echo >> $_output
        fi
    fi
    echo $_exit_code >> $_output
    if [ $_exit_code -ne 0 ]; then
        echo "Non-zero exit code: $_exit_code"
    fi

    # compare output
    if [ ! -f $_ref ]; then
        echo "No reference output for $_suite::$_case"
        return
    fi
    diff -Z $_ref $_output > /dev/null
    if [ $? -ne 0 ]; then
        echo "Unexpected output for $_suite::$_case"
        exit 1
    fi

    echo "Done"
}

_command=$1
case $_command in
    clean)
        clean
        ;;
    make_output_dirs)
        make_output_dirs
        ;;
    compile)
        cargo build
        make_output_dirs
        shift
        compile_test_case $@
        ;;
    compile_all)
        cargo build
        make_output_dirs
        for benchmark in ${benchmarks[@]}; do
            for file in $testbench_root/$benchmark/*.sy; do
                _case=$(basename $file .sy)
                compile_test_case $benchmark $_case
            done
        done
        ;;
    run)
        shift
        run_test_case $@
        ;;
    run_all)
        for benchmark in ${benchmarks[@]}; do
            for file in $testbench_root/$benchmark/*.sy; do
                _case=$(basename $file .sy)
                run_test_case $benchmark $_case
            done
        done
        ;;
    *)
        echo "Usage: testbench.sh <command> [args]"
        echo "Commands:"
        echo "  clean"
        echo "  make_output_dirs"
        echo "  compile <suite> <case> [sysyflags]"
        echo "  compile_all"
        exit 1
        ;;
esac