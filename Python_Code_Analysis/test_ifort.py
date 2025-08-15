# import packages
# Notes: I am trying to only import standard packages in python3
#        so that it can be used for most linux OS

import subprocess # for final system calls
import fileinput # for modifying ED2IN
import xml.etree.ElementTree as xmlET
import xml.dom.minidom as minidom
import argparse # for processing arguments of this script
from pathlib import Path  # deal with path/directories
import time # use the sleep function
import os # use to get current working directory
import numpy as np
import math as math

################################################
# Process script argument
################################################

parser = argparse.ArgumentParser(
    description='Options for submitting ED2 simulations')

# add argument to indicate whether to submit the simulation
# this is an optional argument thus starts with a -,
# The script will ONLY submit the run when -s or --submit is included.
parser.add_argument('--submit','-s',action='store_true')

# add argument to indicate how many cpu cores to use
# by default it uses 20 cores
parser.add_argument('--cpu','-c',action='store',default=40)

# add argument to indicate the total simulation wall time
# default is 120 hours
parser.add_argument('--sim_time','-t',action='store',default="1000:00:00")


# add argument to indicate the queue name
# default is to the shared queue
parser.add_argument('--qname','-q',action='store',default="shared")
# Feel free to add more arguments here
# e.g. output directory etc.


# Parse the argument and get its values
args = parser.parse_args()
# the values of each flag can be accessed through
#    args.submit, args.cpu etc.

#################################################




#################################################
# Define some constants and configuration
#################################################
# I/O
output_dir   = f'/ibstorage/yixin/ED2test/BCI/new_plasticity/test_ifort/'
work_dir = '/home/ym524/ED2test/new_plasticity/'

Path(output_dir).mkdir(parents=True, exist_ok=True)

# create the directory if no already present
# parents=True indicates recursively create any non-existant parent folders
# exist_ok indicates not to throw an error if the folder is already existant.

# ed2 executable to use
ed2_exec = '/home/ym524/ED2test/ED2/ED/build/ed_2.2-opt-new_plasticity-aa62db0'

#----------------------------------------------
# ED2IN information
#----------------------------------------------
# ED2IN template to copy from
ED2IN_template = "/home/ym524/ED2test/ED2IN_template"

# use a dictionary to store all ED2IN flags to be changed
ED2IN_flags_common = {}

# assign flags that are same for all simulations

# runtype and initilization
ED2IN_flags_common['RUNTYPE'] = "'INITIAL'"
ED2IN_flags_common['IED_INIT_MODE'] = '6'

# start and end time time
# 1850 to 2015
ED2IN_flags_common.update({
        'IDATEA' : '01',
        'IMONTHA' : '01',
        'IYEARA' : '1980',
        'ITIMEA' : '0000',
        'IDATEZ' : '01',
        'IMONTHZ' : '03',
        'IYEARZ' : '1980',
        'ITIMEZ' : '0000',
        'MONTH_YRSTEP' : '1',
        'IMONTHH' : '07',
        'IDATEH' : '01',
        'IYEARH' : '1850',
        'ITIMEH' : '0000',
    })

# I/O options
ED2IN_flags_common.update({
        'ISOUTPUT' : '3',
        'IMOUTPUT' : '3',
        'IQOUTPUT' : '3',
        'IYOUTPUT' : '3',
        'ITOUTPUT' : '0',
        'IDOUTPUT' : '3',
        'IFOUTPUT' : '0',
        'UNITSTATE' : '3',
        'FRQSTATE' : '1',
        'FRQFAST' : '3600'
})

# Site information
ED2IN_flags_common['POI_LAT'] = "9.1543"
ED2IN_flags_common['POI_LON'] = "-79.8461"
ED2IN_flags_common['ED_REG_LATMIN'] = "9.1543"
ED2IN_flags_common['ED_REG_LATMAX'] = "9.1543"
ED2IN_flags_common['ED_REG_LONMIN'] = "-79.8461"
ED2IN_flags_common['ED_REG_LONMAX'] = "-79.8461"


ED2IN_flags_common['SLXCLAY'] = "0.68"
ED2IN_flags_common['SLXSAND'] = "0.2"
ED2IN_flags_common['NSLCON'] = "6"
# clay and sand fraction unknown
# use sandy load predefined in the model

ED2IN_flags_common['NZG'] = "16" # 16 soil layers
ED2IN_flags_common['SLZ'] = "-10.00,-9.00,-8.00,-7.00,-6.00,-5.00,-4.00,-3.00,-2.50,-2.00,-1.50,-1.00,-0.60,-0.40,-0.20,-0.10"

# necessary external data bases
ED2IN_flags_common.update({
        'VEG_DATABASE' : "'/ibstorage/shared/ed2_data/oge2OLD/OGE2_'",
        'SOIL_DATABASE' : "'/ibstorage/shared/ed2_data/soil_ed22/Quesada+RADAM+IGBP/Quesada_RADAM_IGBP_'",
        'LU_DATABASE' : "'/ibstorage/shared/ed2_data/land_use/glu+sa1/glu+sa1-'",
        'THSUMS_DATABASE' : "'/ibstorage/shared/ed2_data/ed_inputs/'",
        'SOILSTATE_DB' : "''",
        'SOILDEPTH_DB' : "''",
})

# MET information
ED2IN_flags_common.update(
    {
        'METCYC1'       : '1985',
        'METCYCF'       : '2012',
        'IMETAVG'       : '3',
        'IMETRAD'       : '0',
        'INITIAL_CO2'   : '380.',
    }
)

# physiological parameters
ED2IN_flags_common.update(
    {
        'IPHYSIOL' : '3',
        'TRAIT_PLASTICITY_SCHEME' : '3',
        'ECONOMICS_SCHEME' : '1',
        'IGRASS' : '0',
        'ISTRUCT_GROWTH_SCHEME' : '2',
        'H2O_PLANT_LIM' : '4',
        'PLANT_HYDRO_SCHEME' : '1',
        'ISTOMATA_SCHEME' : '1', # katul's model
        'CARBON_MORTALITY_SCHEME' : '2', # growth-based carbon mortlaity
        'HYDRAULIC_MORTALITY_SCHEME' : '0', # no hydraulic mortality
        'GROWTHRESP' : '0.45',
        'Q10_C3' : '2.4',
        'Q10_C4' : '2.4',
        'INCLUDE_FIRE' : '0',
        'INCLUDE_THESE_PFT' : "2,3,4", # grass +  3 tropical PFTs
        'MAXPATCH' : '20',
        'MAXCOHORT' : '40',
        'TREEFALL_DISTURBANCE_RATE' : '0.00',  # average for Amazon
        'IPHEN_SCHEME' : '4', # hydraulics-driven phenology
        'REPRO_SCHEME' : '3', # continuous function for reproduction
    }
)

# mischellaneous
ED2IN_flags_common['DT_CENSUS'] = 1
ED2IN_flags_common['YR1ST_CENSUS'] = 3200 # maximum possible values, do not include census
ED2IN_flags_common['MIN_RECRUIT_DBH'] = 1 

#----------------------------------------------
################################################



################################################
# Job preparation and  submission
################################################
# different allometry schemes
allom=[4]

##################################################################
# loop over the values and submit one simulation for each value
# !!!! WARNING:
#    Modify ED2IN_flags_run (not ED2IN_flags_common) within the loop
###################################################################

for iallom in allom:
    name = int(iallom)
    job_name = f"test_ifort"

    # create separate folder for each simulation
    sim_dir = output_dir + f'{job_name}/'
    
    Path(sim_dir).mkdir(parents=True, exist_ok=True)
    
    ED2IN_fn = work_dir + f'ED2IN_{job_name}'

    # copy the common ED2IN flags
    ED2IN_flags_run = ED2IN_flags_common.copy()


    # update flags specific to this simulations
    ED2IN_flags_run['IALLOM']=iallom


    ED2IN_flags_run['FFILOUT'] = f"'{sim_dir}{job_name}'"
    ED2IN_flags_run['SFILOUT'] = f"'{sim_dir}{job_name}'"
    # history file
    ED2IN_flags_run['SFILIN'] = "'/home/ym524/ED2test/bci'"

    # update MET HEADER
    ED2IN_flags_run['ED_MET_DRIVER_DB'] = "'/ibstorage/yixin/ED2test/BCI/yixin_BCI_MET_HEADER'"

    # xml
    # create and set xml
    xml_name = f"{work_dir}{job_name}.xml"
    with open(xml_name,'w') as f:
        # create the root layer config
        xml_config = xmlET.Element('config')
        xml_tree = xmlET.ElementTree(xml_config)

        # for each pft create a dictionary
        # halve the mortality
        pfts = [
                {'num' : 2, 'rho' : 0.432, 'b1Ht' : 0.937, 'b2Ht' : 0.593,
                'Vm0' : 31.34, 'Rd0' : 0.3828, 'leaf_turnover_rate' : 1.153, 
                'SLA' : 18.04, 'r_cv50' : 0.5},
                {'num' : 3, 'rho' : 0.672, 'b1Ht' : 0.937, 'b2Ht' : 0.593,
                'Vm0' : 32.23, 'Rd0' : 0.1821, 'leaf_turnover_rate' : 1.124,
                'SLA' : 16.78, 'r_cv50' : 0.5},
                {'num' : 4, 'rho' : 0.826, 'b1Ht' : 0.937, 'b2Ht' : 0.593,
                'Vm0' : 35.58, 'Rd0' : 0.2801, 'leaf_turnover_rate' : 1.597,
                'SLA' : 27.37, 'r_cv50' : 0.5}
            ]
        for pft in pfts:
            pft_xml = xmlET.SubElement(xml_config,'pft')
            for var in pft.keys():
                var_xml = xmlET.SubElement(pft_xml,var)
                var_xml.text = f"{pft[var]}"

        xml_str_pretty = minidom.parseString(xmlET.tostring(xml_tree.getroot(),encoding='unicode')).toprettyxml()

        f.write(xml_str_pretty)

        
    ED2IN_flags_run['IEDCNFGF'] = f"'{xml_name}'"# xml file to use



    #---------------------------------------------
    # create and modify ED2IN
    #---------------------------------------------
    subprocess.run([f'cp {ED2IN_template} {ED2IN_fn}'],shell=True)

    with fileinput.FileInput(ED2IN_fn, inplace=True, backup='.bak') as file:
        for line in file:
            # only update the lines start with NL
            line_no_ws = line.lstrip() # get rid of th leading white space
            num_of_space = len(line) - len(line_no_ws)
            if len(line_no_ws) < 2 or line_no_ws[0:2] != 'NL':
                print(line,end='') # print back the original line
                continue

            # this is a flag line, check whether it contains any of our key in the ED2IN_flags_run
            # get flag name, the first three character is NL%
            flag = line_no_ws.split(' ')[0][3::] # 

            # if present, replace the line
            if flag in ED2IN_flags_run.keys():
                line = " "*num_of_space + f"NL%{flag} = {ED2IN_flags_run[flag]}\n"
            # print back the content
            print(line,end='')
    

    #---------------------------------------------
    # submit job
    #---------------------------------------------

    # command string for sbatch
    cmd_strs = [
        f"ulimit -s unlimited",
        f"export OMP_NUM_THREADS=1",
        f"cd \$SLURM_SUBMIT_DIR",
        f"mpirun --mca btl tcp,self {ed2_exec} -f ED2IN_test_ifort"
    ]
    cmd_str = " ; ".join(cmd_strs)

    #option flags for sbatch
    slurm_opts = [
        f"-o {job_name}.out",
        f"-e {job_name}.err",
        f"-J {job_name}",
        f"-t {args.sim_time}",
        f"--mem-per-cpu=1000",
        f"-n 1",
        f"-c {args.cpu}",
        f"--mail-type=END",
        f"--mail-user=ym524@cornell.edu"
    ]
    slurm_opt = ' '.join(slurm_opts)

    print(slurm_opt)
    print(cmd_str)

    # submit job
    if args.submit:
        # this is compatible with python <3.6
        result = subprocess.run(
            f'sbatch {slurm_opt} --wrap="{cmd_str}"',
            stdout = subprocess.PIPE,shell=True)
        # print the return value (jobid) of the submission
        print(result.stdout,end='')

        # wait for at least 0.1 seconds before the next submission
        # some platform will block too frequent job submissions
        time.sleep(0.1)
    else:
        print("Only a test; Job not submitted. Use -s to submit the jobs")


