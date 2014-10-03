#!/usr/bin/python

## All this code is copyright 2009-2014, Ramon Diaz-Uriarte.
## Released under the Affero GPL.


### For R: inputData.RData and options.txt
### We get from caller a directory name
### We return to caller a directory name
### Files:
##        Status_Server_Run.msg
##        Error_msg.txt

import sys
import os
import cgi 
import time
import shutil
import glob
import random
import socket
import fcntl


## change these tow lines for your setting
appDir       = '/asterias-web-apps/adacgh-server'
Rexec       =  '/asterias-web-apps/R-3.1.1-patched-2014-08-21/bin/R'

#########################

R_MAX_time = 24 * 3600 ## 1 days is max duration allowd for any process
TIME_BETWEEN_CHECKS = 5
MAX_MPI_CRASHES = 1
MAX_NUM_PROCS = 30 # my_queue
MAX_DURATION_TRY = 24 * 3600 # my_queue
## directory structure; change if you want, but no need for it
appProcs     = appDir + '/runs-tmp'
logsDir      = appProcs + '/logs'
runningProcs = appProcs + '/R.running.procs'


tmpDir = sys.argv[1]


def add_to_log(applicacion, tmpDir, hostname):
    date_time = time.strftime('%Y\t%m\t%d\t%X')    
    outstr = '%s\t%s\t%s\t%s\n' % (applicacion, date_time, hostname, tmpDir)
    cf = open(logsDir + '/ApplicationCounter', mode = 'a')
    fcntl.flock(cf.fileno(), fcntl.LOCK_SH)
    cf.write(outstr)
    fcntl.flock(cf.fileno(), fcntl.LOCK_UN)
    cf.close()

    
def writeStatusServer(message, outfile = 'Status_Server_Run.msg'):
    ## we take tmpDir as a given! from local envir
    statusfile = open(tmpDir + '/' + outfile, mode = 'w')
    statusfile.write(message)
    statusfile.close()


def writeErrorMessage(message = '', infile = 'R_Error_msg.txt',
                      outfile = 'Error_msg.txt'):
    """ Write the error message for users by 
    concatenating the message and the infile."""
    if (message == '') and (infile != 'NULL'):
        shutil.copyfile(tmpDir + '/' + infile,
                        tmpDir + '/Error_msg.txt')
    elif (message != '') and (infile == 'NULL'):
        errorfile = open(tmpDir + '/' + outfile, mode = 'w')
        errorfile.write(message)
        errorfile.close()
    elif (message != '') and (infile != 'NULL'):
        errorfile = open(tmpDir + '/' + outfile, mode = 'w')
        infr = open(tmpDir + '/' + infile, mode = 'r').read()
        errorfile.write(message +'\n\n\n' + infr)
        errorfile.close()
    elif (message == '') and (infile == 'NULL'):
        pass

    
# def create_tmpDir(baseDir = tmpDD):
#     """ Create a new directory with appropriate permissions"""
#     newDir = str(random.randint(1, 10000000)) + str(int(time.time())) + str(os.getpid())
#     tmpDir = baseDir + "/" + newDir
#     os.mkdir(tmpDir)
#     os.chmod(tmpDir, 0700)
#     return (newDir, tmpDir)

def issue_echo(fecho, tmpDir):
    """Silly function to output small tracking files"""
    timeHuman = '##########   ' + \
                str(time.strftime('%d %b %Y %H:%M:%S')) 
    os.system('echo "' + timeHuman + \
              '" >> ' + tmpDir + '/checkdone.echo')
    os.system('echo "' + fecho + \
              '" >> ' + tmpDir + '/checkdone.echo')
    os.system('echo "    " >> ' + tmpDir + '/checkdone.echo')


def kill_pid_machine(pid):
    'as it says: to kill somehting and not get error messages'
    fi, fo, fu = os.popen3('kill -s 9 ' + pid )
    fi.close()
    fo.close()
    fu.close()




def generic_crash_log(tmpDir, value):
    """ Write to the crash log, 'recoverFromCrash.out' """
    timeHuman = str(time.strftime('%d %b %Y %H:%M:%S')) 
    os.system('echo "' + value + '  at ' + timeHuman + \
              '" >> ' + tmpDir + '/recoverFromCrash.out')

def Rrun(tmpDir, Rexec = Rexec):
    """ Launch R."""
    issue_echo(' inside Rrun ', tmpDir)
    Rcommand = 'cd ' + tmpDir + \
               '; sleep 1; ' + \
               Rexec + ' --no-readline --no-save --slave <f4.R >>f4.Rout 2>> Status.msg &'
    issue_echo('the Rcommand is ' + Rcommand, tmpDir)
    Rtorun = os.system(Rcommand)

    
def status_run(tmpDir):
    """ Read R_Status.txt and return status."""
    status_r = open(tmpDir + '/R_Status.txt').read()
    if status_r.find('Execution halted\n') > -1:
        return('R_ExecutionHalted')
    if status_r.find('Other Error\n') > -1:
        return('R_Other_Error')
    if status_r.find('User Error\n') > -1:
        return('R_User_Error')
    if status_r.find('Our Error\n') > -1:
        return('R_Our_Error')
    if status_r.find('Rmpi error\n') > -1:
        return('Error_Rmpi')
    if status_r.find('Running\n') > -1:
        return('Running')
    if status_r.find('Normal termination\n') > -1:
        return('R_NormalTermination')
#     if status_r.find('Run out of time; killed\n') > -1:
#         return('Out_of_time')



def did_run_out_of_time(tmpDir, R_MAX_time):
    """ Did the process run longer than allowed?"""
    issue_echo('did we run out of time?', tmpDir)
    if not os.path.exists(tmpDir + "/pid.txt"):
        return False
    elif ((time.time() - os.path.getmtime(tmpDir + "/pid.txt")) > R_MAX_time) and \
       (status_run(tmpDir) == 'Running'):
        return True
    else:
        return False
                           

def cleanups(tmpDir, newDir,
             runningProcs= runningProcs,
             newnamepid = 'finished_pid.txt'):
    """ Clean up actions; kill lam, delete running.procs files, clean process table."""
    try:
        rinfo = open(tmpDir + '/current_R_proc_info', mode = 'r').readline().split()
    except:
        None
    try:
        killpidmachine = kill_pid_machine(rinfo[1])
    except:
        None
    try:
        os.system('rm ' + runningProcs + '/R.' + newDir + '*')
    except:
        None
    try:
        os.rename(tmpDir + '/pid.txt', tmpDir + '/' + newnamepid)
    except:
        None


def master_out_of_time(time_start):
    """If this process run longer than allowed, kill it and kill lam and R."""
    if (time.time () - time_start) > R_MAX_time:
        return True
    else:
        return False
        
def my_queue(MAX_NUM_PROCS,
             runningProcs = runningProcs,
             CHECK_QUEUE = 5,
             MAX_DURATION_TRY = MAX_DURATION_TRY):
    """ Wait here until the number of processes is smaller than
    MAX_NUM_PROCS 
    But only wait for MAX_DURATION. Check
    every CHECK_QUEUE seconds. If able to find an opening, return
    OK, otherwise return Failed"""

    # taken from the smae function in signs
    out_value = 'OK'
    startTime = time.time()
    while True:
        issue_echo('     inside my_queue ', tmpDir)
        if (time.time() - startTime) > MAX_DURATION_TRY:
            out_value = 'Failed'
            break
        num_sentinel = int(len(glob.glob(''.join([runningProcs, 'sentinel.lam.*']))))
        if (num_sentinel < MAX_NUM_PROCS):
            issue_echo(' num_sentinel = ' + str(num_sentinel), tmpDir)
            break
        else:
	    issue_echo('     wait:  ' + \
                       '; num_sentinel = ' + str(num_sentinel), tmpDir)
            time.sleep(CHECK_QUEUE + random.uniform(0.1, 5))
    return out_value
    

######################################################################
######################################################################
######################################################################

## Starting. First, the very first run.
tmp1 = tmpDir.split('/')
newDir = tmp1[len(tmp1) - 1]


add_to_log("Wavi", tmpDir, socket.gethostname())
print tmpDir

os.system('echo "' + str(os.getpid()) + ' ' + socket.gethostname() +\
           '"> ' + tmpDir + '/run_and_checkPID')


issue_echo('starting', tmpDir)
writeStatusServer('Running')


shutil.copyfile(appDir + '/f4.R',
                tmpDir + '/f4.R')
os.system('/bin/touch ' + tmpDir + '/R_Error_msg.txt')
touchRout = os.system("/bin/touch " + tmpDir + "/f4.Rout") 
touchRrunning = os.system('/bin/touch ' +
                          runningProcs + '/R.' + newDir +
                          "@" + socket.gethostname())
checkpoint = os.system("echo 0 > " + tmpDir + "/checkpoint.num")

       
issue_echo('at 2', tmpDir)

issue_echo('at 3', tmpDir)

time.sleep(random.uniform(0.1, 3)) ## Break ties if starting at identical times

issue_echo('at 4', tmpDir)


check_room = my_queue(MAX_NUM_PROCS)
issue_echo('after check_room', tmpDir)


## start R
Rrun(tmpDir, Rexec)
issue_echo('         at          A1', tmpDir)
        
time_start = time.time()
## it takes a few seconds to get R_Status.txt
time.sleep(TIME_BETWEEN_CHECKS + random.uniform(0.1, 3))

count_mpi_crash = 0

### FIXME: maybe we can get rid of the issue_echo calls


while True:
    if (status_run(tmpDir) == 'R_NormalTermination'):
        issue_echo(status_run(tmpDir), tmpDir)
        cleanups(tmpDir, newDir)
        writeStatusServer('Normal termination\n')
        ### FIXME: what about PaLS, etc? See old "printOKRun()"
        break
    elif (status_run(tmpDir) == 'R_User_Error'):
        
        issue_echo(status_run(tmpDir), tmpDir)
        cleanups(tmpDir, newDir)
        writeStatusServer('User ERROR\n')
        writeErrorMessage('', 'R_Error_msg.txt')
        break
    elif (status_run(tmpDir) == 'R_ExecutionHalted'):
        issue_echo(status_run(tmpDir), tmpDir)
        cleanups(tmpDir, newDir)
        writeStatusServer('ERROR!!!\n')
        writeErrorMessage('', 'R_Error_msg.txt')
        break
    elif (status_run(tmpDir) == 'R_Other_Error'):
        issue_echo(status_run(tmpDir), tmpDir)
        cleanups(tmpDir, newDir)
        writeStatusServer('ERROR!!!\n')
        writeErrorMessage('', 'R_Error_msg.txt')
        break
    elif (status_run(tmpDir) == 'R_Our_Error'):
        issue_echo(status_run(tmpDir), tmpDir)
        cleanups(tmpDir, newDir)
        writeStatusServer('ERROR!!!\n')
        writeErrorMessage('', 'R_Error_msg.txt')
        break

    elif master_out_of_time(time_start):
        issue_echo('Master out of time', tmpDir)
        cleanups(tmpDir, newDir)
        writeStatusServer('ERROR!!!\n')
        writeErrorMessage('Master out of time', 'NULL')
        break

    elif did_run_out_of_time(tmpDir, R_MAX_time):
        issue_echo('R run out of time', tmpDir)
        cleanups(tmpDir, newDir)
        writeStatusServer('ERROR!!!\n')
        writeErrorMessage('R run out of time', 'NULL')
        break
    else:
        generic_crash_log(tmpDir, 'NoCrash') ## if we get here, this much we know
    time.sleep(TIME_BETWEEN_CHECKS)


issue_echo('before clean up', tmpDir)

issue_echo('at the very end!', tmpDir)





