## BioHPC Environment
### System Overview
- This system is part of the Cornell BioHPC Cloud environment.
- Each user's home directory (/home/$USER) is located on a shared network-mounted Lustre filesystem.
- Each server provides a local /workdir filesystem that is physically attached to that server and is generally preferred for active computation, temporary files, intermediate results, and large datasets.
- Unless otherwise specified, assume users connect to the server via SSH and run software directly on the server.

### Long-Running Jobs
- For interactive work and short analyses, users may run commands directly from an SSH session.
- For long-running jobs executed directly on the server (without Slurm), recommend using a persistent terminal session such as screen.
- If a command is expected to run longer than the user's SSH session, suggest creating a screen session before starting the job.
- Do not assume that a long-running command will survive an SSH disconnect unless it is running inside screen, tmux, or a similar session manager.
- Screen tutorial can be found at https://biohpc.cornell.edu/lab/doc/Linux_exercise_part2.pdf

## Working Directory

- Store all working files, intermediate results, and large datasets under `/workdir/$USER`. Do not use your home directory (`/home/$USER`) for compute-intensive workflows or large files.
- If software requires a large temporary directory, do not use `/tmp`, as it may have limited capacity and is shared across users.
- Instead, create a dedicated temporary directory under your work area, for example:

  ```bash
  mkdir -p /workdir/$USER/tmp
  ```

## Docker Environment

### Docker Command Requirements
- Do not use the docker command directly on this server.
- Use docker1 for all Docker operations.
- docker1 is a site-specific wrapper that executes Docker commands with the required privileges (equivalent to sudo docker).
- For any Docker-related task, always substitute docker1 for docker.
- Examples:
  - `docker1 images`
  - `docker1 ps`
  - `docker1 build ...`
  - `docker1 run ...`

### Container Filesystem Restrictions

- Containers may only mount directories under /workdir/$USER.
- For this account, paths under /workdir/$USER are permitted.
- Never mount directories from /home, /tmp, /usr, /etc, or any location outside /workdir/$USER unless explicitly instructed by the user.
- Examples:
  - docker1 run -v /workdir/$USER/project:/workspace ...



## Installed Software Lookup

### General Software information
- Before installing software, check this web page first: https://biohpc.cornell.edu/lab/userguide.aspx?a=software. 

- In this HTML page, all software names are listed. For each software package, follow the linked documentation page for instructions on loading modules and running the software.

### Nextflow
- Before running any Nextflow workflow, load an appropriate Nextflow module: module load nextflow/25.4.3

- Available Nextflow versions include:
  - 25.4.3 (preferred)
  - 24.10.1
  - 23.10.1
  - 22.10.7
  - 19.04.1

- When possible, run Nextflow workflows using Apptainer or Singularity containers.
- Singularity 1.4.0-1.el9 is already installed. Do not spend time checking whether Singularity or Apptainer is available.
- Unless the user explicitly requests Slurm execution, run Nextflow workflows locally on the server rather than submitting jobs through Slurm.

### Conda
- General Guidance
  - Conda should not be the default method for installing software on BioHPC.
  - When possible, prefer:
    - Software already installed on BioHPC
    - Docker or Apptainer/Singularity containers

  - Conda Installation Location
    - For users with access to a hosted server
      - install Miniconda and create the Conda base environment under /workdir/$USER.
    - For users without access to a hosted server:
      - Install Miniconda and create the Conda base environment in the default location under the home directory.

  - Recommended Distribution
    - Miniconda is the recommended Conda distribution.
    - Avoid installing the full Anaconda distribution unless specifically required.

  - BioHPC Conda installation and usage instructions: https://biohpc.cornell.edu/lab/userguide.aspx?a=software&i=574#c

### Apptainer
- Apptainer is installed and available in the default PATH.
- Singularity compatibility is available and the singularity command is also available in the default PATH.

### Python
- Default Python Environment
  - The default Python installation is Python 3.9.25.

  - The following commands are available in the default PATH and use Python 3.9.25:
    - python
    - python3
    - pip
    - pip3

  - Unless otherwise specified, assume that Python-related commands, package installations, and virtual environments use the default Python 3.9.25 installation.
- Switching Python Versions
  - To use a different Python version, load the appropriate module before running Python commands.

  - Example:
    - module load python/3.12.7
    - After loading a Python module, use the corresponding python, python3, pip, and pip3 commands from that environment.

- Available Python Versions
  - 3.12.7
  - 3.10.6-r9
  - 3.9.25 (default)
  - 3.6.15-r9
  - 2.7.15
  - 2.7.5

- Python Package Installation
  - Before installing Python packages, determine whether the required package is already available through the system Python installation or a module.
  - User can install python packages in default path, under ~/.local

### R
- Default R Environment
  - The default R installation is R 4.0.5.
  - The following commands are available in the default PATH and use R 4.0.5:
    - R
    - Rscript

- Switching R Versions
  - To use a different R version, load the appropriate module before running R commands.

  - Example:
    - module load R/4.5.2
    - After loading a R module, use the corresponding R, Rscript.

- Available R Versions
    - 4.0.5-r9  
    - 4.1.3-r9
    - 4.2.3
    - 4.3.3
    - 4.4.2
    - 4.4.3
    - 4.5.2

### R studio 
- There are three supported ways to start RStudio on BioHPC:

  - RStudio Server directly on the host computer. This is the default method.
  - RStudio Server through Docker. This is recommended only for experienced users.
  - RStudio Desktop through VNC. This method is not recommended unless specifically needed.

  - Instructions for all three methods are available here:

    - https://biohpc.cornell.edu/lab/userguide.aspx?a=software&i=266#c

- R Versions: 
  - Multiple R versions are supported.
  - Check the BioHPC RStudio instruction page for the currently supported R versions and how to select them.

- Required Setup for Host RStudio Server
  - When using RStudio Server directly on the host computer, each user should run the following command before starting or troubleshooting RStudio:
    - /programs/rstudio_server/mv_dir
  - This command performs two setup actions:
    - Removes cached RStudio session files.
    - Moves the RStudio cache directory under /workdir/$USER and creates a symbolic link from ~/.local/share/rstudio.
    
- Troubleshooting
  - For RStudio troubleshooting, refer to the BioHPC FAQ: https://biohpc.cornell.edu/doc/UsingRstudioServer.html

### Java
- Default Java Environment
  - The default Java installation is openjdk 13.0.2.
  - The following commands are available in the default PATH and use open jdk 13.0.2:
    - java

- Switching java Versions
  - To use a different java version, load the appropriate module before running java commands.

  - Example:
    - module load java/21.0.1
    - After loading a java module, use the corresponding java command.

- Available Java Versions
    - 13.0.2 (default)
    - 1.7.0
    - 1.8.0
    - 21.0.1


## Parallelization

- Determine whether the user intends to run jobs through Slurm or directly on a login/workstation server.

- If a user is processing lots of files, or any tasks that are parallelizable, suggest user to do parallelization.

- If an application is multi-threaded, suggest users to set CPU cores.

- If the user is using Slurm:
  - Prefer Slurm-native job parallelization methods (job arrays, multiple jobs, workflow managers, etc.).
  - Do not recommend GNU Parallel unless the user specifically requests it.

- If the user is running jobs locally on a server without Slurm:
  - Prefer GNU Parallel for command-line job parallelization.
  - GNU Parallel is installed and available in the default PATH.
  - Do not suggest installing alternative parallelization tools unless specifically requested.

- If the execution environment is unclear, ask whether the workload will be run through Slurm or directly on the server before proposing a parallelization strategy.


## AI Agent behavior
- Do not automatically execute pipelines, workflows, batch jobs, or long-running computational tasks.
- Instead, generate a shell script containing the required commands and save the workflow in that script.
- Present the script to the user and instruct them to review it before execution.
- Assume that all substantial compute jobs should be launched manually by the user in a separate terminal session.
- Do not start Nextflow workflows, Slurm jobs, Docker containers, Apptainer containers, or other long-running processes unless the user explicitly requests execution.
- When possible, separate workflow preparation (performed by the agent) from workflow execution (performed by the user).
- For potentially expensive jobs, provide the exact command or script needed and explain how the user can run it.



