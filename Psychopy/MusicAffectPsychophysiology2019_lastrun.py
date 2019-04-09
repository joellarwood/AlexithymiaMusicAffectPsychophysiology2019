#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
This experiment was created using PsychoPy3 Experiment Builder (v3.0.5),
    on Tue Apr  9 10:46:08 2019
If you publish work using this script please cite the PsychoPy publications:
    Peirce, JW (2007) PsychoPy - Psychophysics software in Python.
        Journal of Neuroscience Methods, 162(1-2), 8-13.
    Peirce, JW (2009) Generating stimuli for neuroscience using PsychoPy.
        Frontiers in Neuroinformatics, 2:10. doi: 10.3389/neuro.11.010.2008
"""

from __future__ import absolute_import, division

import psychopy
psychopy.useVersion('latest')

from psychopy import locale_setup, sound, gui, visual, core, data, event, logging, clock
from psychopy.constants import (NOT_STARTED, STARTED, PLAYING, PAUSED,
                                STOPPED, FINISHED, PRESSED, RELEASED, FOREVER)
import numpy as np  # whole numpy lib is available, prepend 'np.'
from numpy import (sin, cos, tan, log, log10, pi, average,
                   sqrt, std, deg2rad, rad2deg, linspace, asarray)
from numpy.random import random, randint, normal, shuffle
import os  # handy system and path functions
import sys  # to get file system encoding


# Ensure that relative paths start from the same directory as this script
_thisDir = os.path.dirname(os.path.abspath(__file__))
os.chdir(_thisDir)

# Store info about the experiment session
psychopyVersion = '3.0.5'
expName = 'MusicAffectPsychophysiology2019'  # from the Builder filename that created this script
expInfo = {'participant': '', 'session': '001'}
dlg = gui.DlgFromDict(dictionary=expInfo, title=expName)
if dlg.OK == False:
    core.quit()  # user pressed cancel
expInfo['date'] = data.getDateStr()  # add a simple timestamp
expInfo['expName'] = expName
expInfo['psychopyVersion'] = psychopyVersion

# Data file name stem = absolute path + name; later add .psyexp, .csv, .log, etc
filename = _thisDir + os.sep + u'data/%s_%s_%s' % (expInfo['participant'], expName, expInfo['date'])

# An ExperimentHandler isn't essential but helps with data saving
thisExp = data.ExperimentHandler(name=expName, version='',
    extraInfo=expInfo, runtimeInfo=None,
    originPath='/Users/uqjlarwo/Desktop/git/AlexithymiaMusicAffectPsychophysiology2019/Psychopy/MusicAffectPsychophysiology2019_lastrun.py',
    savePickle=True, saveWideText=True,
    dataFileName=filename)
# save a log file for detail verbose info
logFile = logging.LogFile(filename+'.log', level=logging.EXP)
logging.console.setLevel(logging.WARNING)  # this outputs to the screen, not a file

endExpNow = False  # flag for 'escape' or other condition => quit the exp

# Start Code - component code to be run before the window creation

# Setup the Window
win = visual.Window(
    size=[1440, 900], fullscr=True, screen=0,
    allowGUI=False, allowStencil=False,
    monitor='testMonitor', color=[0,0,0], colorSpace='rgb',
    blendMode='avg', useFBO=True)
# store frame rate of monitor if we can measure it
expInfo['frameRate'] = win.getActualFrameRate()
if expInfo['frameRate'] != None:
    frameDur = 1.0 / round(expInfo['frameRate'])
else:
    frameDur = 1.0 / 60.0  # could not measure, so guess

# Initialize components for Routine "instru"
instruClock = core.Clock()
instructions = visual.TextStim(win=win, name='instructions',
    text='You are about to listen to a series of songs. After listening to each song you will be asked to make judgements about how the song made you feel in terms of pleasantness and arousal (energy/awakeness). \n\nFirst, you will complete a practice trial so you can get an understanding of what you will be doing in the experiment\n\nPlease press any key to continue',
    font='Arial',
    pos=(0, 0), height=0.1, wrapWidth=None, ori=0, 
    color='white', colorSpace='rgb', opacity=1, 
    languageStyle='LTR',
    depth=0.0);

# Initialize components for Routine "PracMusic"
PracMusicClock = core.Clock()
practice = sound.Sound('prac.wav', secs=-1, stereo=True)
practice.setVolume(2)
import serial
port = serial.Serial(port = "/dev/tty.usbserial-BBTKUSBTTL", 
                    baudrate = 115200, 
                    timeout=1) 

# Initialize components for Routine "arousal"
arousalClock = core.Clock()
arousaltext = visual.TextStim(win=win, name='arousaltext',
    text='From 1 (not at all) to 5 (very much so) how aroused/energised did you feel after listening to this song?\n\n1       2       3       4      5',
    font='Arial',
    pos=(0, 0), height=0.1, wrapWidth=None, ori=0, 
    color='white', colorSpace='rgb', opacity=1, 
    languageStyle='LTR',
    depth=0.0);

# Initialize components for Routine "valence"
valenceClock = core.Clock()
valencetext = visual.TextStim(win=win, name='valencetext',
    text='From 1 (not at all) to 5 (very much so) how positive/pleasant did you feel after listening to this song?\n\n1       2       3       4      5',
    font='Arial',
    pos=(0, 0), height=0.1, wrapWidth=None, ori=0, 
    color='white', colorSpace='rgb', opacity=1, 
    languageStyle='LTR',
    depth=0.0);

# Initialize components for Routine "Differentiation"
DifferentiationClock = core.Clock()
WordInstruction = visual.TextStim(win=win, name='WordInstruction',
    text='default text',
    font='Arial',
    pos=(0, .5), height=0.1, wrapWidth=None, ori=0, 
    color='white', colorSpace='rgb', opacity=1, 
    languageStyle='LTR',
    depth=0.0);
WordScale = visual.TextStim(win=win, name='WordScale',
    text='1 (Not at all)\n\n2 (Little)\n\n3 (Much)\n\n4(A great deal)',
    font='Arial',
    pos=(0, -.3), height=0.1, wrapWidth=None, ori=0, 
    color='white', colorSpace='rgb', opacity=1, 
    languageStyle='LTR',
    depth=-2.0);
EmoWord = visual.TextStim(win=win, name='EmoWord',
    text='default text',
    font='Arial',
    pos=(0, .3), height=0.1, wrapWidth=None, ori=0, 
    color='white', colorSpace='rgb', opacity=1, 
    languageStyle='LTR',
    depth=-3.0);

# Initialize components for Routine "beginstudy"
beginstudyClock = core.Clock()
text = visual.TextStim(win=win, name='text',
    text='Now that the practice is finished the experiment will begin. This will take about 15 minutes. You will be told by the program when the experiment is over\n\nPress any key to begin the study',
    font='Arial',
    pos=(0, 0), height=0.1, wrapWidth=None, ori=0, 
    color='white', colorSpace='rgb', opacity=1, 
    languageStyle='LTR',
    depth=0.0);

# Initialize components for Routine "music"
musicClock = core.Clock()
playsong = sound.Sound('A', secs=-1, stereo=True)
playsong.setVolume(2)


# Initialize components for Routine "arousal"
arousalClock = core.Clock()
arousaltext = visual.TextStim(win=win, name='arousaltext',
    text='From 1 (not at all) to 5 (very much so) how aroused/energised did you feel after listening to this song?\n\n1       2       3       4      5',
    font='Arial',
    pos=(0, 0), height=0.1, wrapWidth=None, ori=0, 
    color='white', colorSpace='rgb', opacity=1, 
    languageStyle='LTR',
    depth=0.0);

# Initialize components for Routine "valence"
valenceClock = core.Clock()
valencetext = visual.TextStim(win=win, name='valencetext',
    text='From 1 (not at all) to 5 (very much so) how positive/pleasant did you feel after listening to this song?\n\n1       2       3       4      5',
    font='Arial',
    pos=(0, 0), height=0.1, wrapWidth=None, ori=0, 
    color='white', colorSpace='rgb', opacity=1, 
    languageStyle='LTR',
    depth=0.0);

# Initialize components for Routine "Differentiation"
DifferentiationClock = core.Clock()
WordInstruction = visual.TextStim(win=win, name='WordInstruction',
    text='default text',
    font='Arial',
    pos=(0, .5), height=0.1, wrapWidth=None, ori=0, 
    color='white', colorSpace='rgb', opacity=1, 
    languageStyle='LTR',
    depth=0.0);
WordScale = visual.TextStim(win=win, name='WordScale',
    text='1 (Not at all)\n\n2 (Little)\n\n3 (Much)\n\n4(A great deal)',
    font='Arial',
    pos=(0, -.3), height=0.1, wrapWidth=None, ori=0, 
    color='white', colorSpace='rgb', opacity=1, 
    languageStyle='LTR',
    depth=-2.0);
EmoWord = visual.TextStim(win=win, name='EmoWord',
    text='default text',
    font='Arial',
    pos=(0, .3), height=0.1, wrapWidth=None, ori=0, 
    color='white', colorSpace='rgb', opacity=1, 
    languageStyle='LTR',
    depth=-3.0);

# Initialize components for Routine "end"
endClock = core.Clock()
MusicEnd = visual.TextStim(win=win, name='MusicEnd',
    text='All the music has now been listened to. Please click on the following link to fill out some final questions.\n\nPress any key to continue',
    font='Arial',
    pos=(0, 0), height=0.1, wrapWidth=None, ori=0, 
    color='white', colorSpace='rgb', opacity=1, 
    languageStyle='LTR',
    depth=0.0);

# Create some handy timers
globalClock = core.Clock()  # to track the time since experiment started
routineTimer = core.CountdownTimer()  # to track time remaining of each (non-slip) routine 

# ------Prepare to start Routine "instru"-------
t = 0
instruClock.reset()  # clock
frameN = -1
continueRoutine = True
# update component parameters for each repeat
begin = event.BuilderKeyResponse()
# keep track of which components have finished
instruComponents = [instructions, begin]
for thisComponent in instruComponents:
    if hasattr(thisComponent, 'status'):
        thisComponent.status = NOT_STARTED

# -------Start Routine "instru"-------
while continueRoutine:
    # get current time
    t = instruClock.getTime()
    frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
    # update/draw components on each frame
    
    # *instructions* updates
    if t >= 0.0 and instructions.status == NOT_STARTED:
        # keep track of start time/frame for later
        instructions.tStart = t
        instructions.frameNStart = frameN  # exact frame index
        instructions.setAutoDraw(True)
    
    # *begin* updates
    if t >= 0.0 and begin.status == NOT_STARTED:
        # keep track of start time/frame for later
        begin.tStart = t
        begin.frameNStart = frameN  # exact frame index
        begin.status = STARTED
        # keyboard checking is just starting
        win.callOnFlip(begin.clock.reset)  # t=0 on next screen flip
        event.clearEvents(eventType='keyboard')
    if begin.status == STARTED:
        theseKeys = event.getKeys()
        
        # check for quit:
        if "escape" in theseKeys:
            endExpNow = True
        if len(theseKeys) > 0:  # at least one key was pressed
            begin.keys = theseKeys[-1]  # just the last key pressed
            begin.rt = begin.clock.getTime()
            # a response ends the routine
            continueRoutine = False
    
    # check for quit (typically the Esc key)
    if endExpNow or event.getKeys(keyList=["escape"]):
        core.quit()
    
    # check if all components have finished
    if not continueRoutine:  # a component has requested a forced-end of Routine
        break
    continueRoutine = False  # will revert to True if at least one component still running
    for thisComponent in instruComponents:
        if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
            continueRoutine = True
            break  # at least one component has not yet finished
    
    # refresh the screen
    if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
        win.flip()

# -------Ending Routine "instru"-------
for thisComponent in instruComponents:
    if hasattr(thisComponent, "setAutoDraw"):
        thisComponent.setAutoDraw(False)
# check responses
if begin.keys in ['', [], None]:  # No response was made
    begin.keys=None
thisExp.addData('begin.keys',begin.keys)
if begin.keys != None:  # we had a response
    thisExp.addData('begin.rt', begin.rt)
thisExp.nextEntry()
# the Routine "instru" was not non-slip safe, so reset the non-slip timer
routineTimer.reset()

# ------Prepare to start Routine "PracMusic"-------
t = 0
PracMusicClock.reset()  # clock
frameN = -1
continueRoutine = True
# update component parameters for each repeat
practice.setSound('prac.wav')
practice.setVolume(2, log=False)
port.write('10'.encode())

# keep track of which components have finished
PracMusicComponents = [practice]
for thisComponent in PracMusicComponents:
    if hasattr(thisComponent, 'status'):
        thisComponent.status = NOT_STARTED

# -------Start Routine "PracMusic"-------
while continueRoutine:
    # get current time
    t = PracMusicClock.getTime()
    frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
    # update/draw components on each frame
    # start/stop practice
    if t >= 0 and practice.status == NOT_STARTED:
        # keep track of start time/frame for later
        practice.tStart = t
        practice.frameNStart = frameN  # exact frame index
        win.callOnFlip(practice.play)  # screen flip
    
    
    # check for quit (typically the Esc key)
    if endExpNow or event.getKeys(keyList=["escape"]):
        core.quit()
    
    # check if all components have finished
    if not continueRoutine:  # a component has requested a forced-end of Routine
        break
    continueRoutine = False  # will revert to True if at least one component still running
    for thisComponent in PracMusicComponents:
        if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
            continueRoutine = True
            break  # at least one component has not yet finished
    
    # refresh the screen
    if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
        win.flip()

# -------Ending Routine "PracMusic"-------
for thisComponent in PracMusicComponents:
    if hasattr(thisComponent, "setAutoDraw"):
        thisComponent.setAutoDraw(False)
practice.stop()  # ensure sound has stopped at end of routine
port.write("00".encode())
# the Routine "PracMusic" was not non-slip safe, so reset the non-slip timer
routineTimer.reset()

# ------Prepare to start Routine "arousal"-------
t = 0
arousalClock.reset()  # clock
frameN = -1
continueRoutine = True
# update component parameters for each repeat
arousalkey = event.BuilderKeyResponse()
# keep track of which components have finished
arousalComponents = [arousaltext, arousalkey]
for thisComponent in arousalComponents:
    if hasattr(thisComponent, 'status'):
        thisComponent.status = NOT_STARTED

# -------Start Routine "arousal"-------
while continueRoutine:
    # get current time
    t = arousalClock.getTime()
    frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
    # update/draw components on each frame
    
    # *arousaltext* updates
    if t >= 0.0 and arousaltext.status == NOT_STARTED:
        # keep track of start time/frame for later
        arousaltext.tStart = t
        arousaltext.frameNStart = frameN  # exact frame index
        arousaltext.setAutoDraw(True)
    
    # *arousalkey* updates
    if t >= 0.0 and arousalkey.status == NOT_STARTED:
        # keep track of start time/frame for later
        arousalkey.tStart = t
        arousalkey.frameNStart = frameN  # exact frame index
        arousalkey.status = STARTED
        # keyboard checking is just starting
        win.callOnFlip(arousalkey.clock.reset)  # t=0 on next screen flip
        event.clearEvents(eventType='keyboard')
    if arousalkey.status == STARTED:
        theseKeys = event.getKeys(keyList=['1', '2', '3', '4', '5'])
        
        # check for quit:
        if "escape" in theseKeys:
            endExpNow = True
        if len(theseKeys) > 0:  # at least one key was pressed
            arousalkey.keys = theseKeys[-1]  # just the last key pressed
            arousalkey.rt = arousalkey.clock.getTime()
            # a response ends the routine
            continueRoutine = False
    
    # check for quit (typically the Esc key)
    if endExpNow or event.getKeys(keyList=["escape"]):
        core.quit()
    
    # check if all components have finished
    if not continueRoutine:  # a component has requested a forced-end of Routine
        break
    continueRoutine = False  # will revert to True if at least one component still running
    for thisComponent in arousalComponents:
        if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
            continueRoutine = True
            break  # at least one component has not yet finished
    
    # refresh the screen
    if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
        win.flip()

# -------Ending Routine "arousal"-------
for thisComponent in arousalComponents:
    if hasattr(thisComponent, "setAutoDraw"):
        thisComponent.setAutoDraw(False)
# check responses
if arousalkey.keys in ['', [], None]:  # No response was made
    arousalkey.keys=None
thisExp.addData('arousalkey.keys',arousalkey.keys)
if arousalkey.keys != None:  # we had a response
    thisExp.addData('arousalkey.rt', arousalkey.rt)
thisExp.nextEntry()
# the Routine "arousal" was not non-slip safe, so reset the non-slip timer
routineTimer.reset()

# ------Prepare to start Routine "valence"-------
t = 0
valenceClock.reset()  # clock
frameN = -1
continueRoutine = True
# update component parameters for each repeat
valencekey = event.BuilderKeyResponse()
# keep track of which components have finished
valenceComponents = [valencetext, valencekey]
for thisComponent in valenceComponents:
    if hasattr(thisComponent, 'status'):
        thisComponent.status = NOT_STARTED

# -------Start Routine "valence"-------
while continueRoutine:
    # get current time
    t = valenceClock.getTime()
    frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
    # update/draw components on each frame
    
    # *valencetext* updates
    if t >= 0.0 and valencetext.status == NOT_STARTED:
        # keep track of start time/frame for later
        valencetext.tStart = t
        valencetext.frameNStart = frameN  # exact frame index
        valencetext.setAutoDraw(True)
    
    # *valencekey* updates
    if t >= 0.0 and valencekey.status == NOT_STARTED:
        # keep track of start time/frame for later
        valencekey.tStart = t
        valencekey.frameNStart = frameN  # exact frame index
        valencekey.status = STARTED
        # keyboard checking is just starting
        win.callOnFlip(valencekey.clock.reset)  # t=0 on next screen flip
        event.clearEvents(eventType='keyboard')
    if valencekey.status == STARTED:
        theseKeys = event.getKeys(keyList=['1', '2', '3', '4', '5'])
        
        # check for quit:
        if "escape" in theseKeys:
            endExpNow = True
        if len(theseKeys) > 0:  # at least one key was pressed
            valencekey.keys = theseKeys[-1]  # just the last key pressed
            valencekey.rt = valencekey.clock.getTime()
            # a response ends the routine
            continueRoutine = False
    
    # check for quit (typically the Esc key)
    if endExpNow or event.getKeys(keyList=["escape"]):
        core.quit()
    
    # check if all components have finished
    if not continueRoutine:  # a component has requested a forced-end of Routine
        break
    continueRoutine = False  # will revert to True if at least one component still running
    for thisComponent in valenceComponents:
        if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
            continueRoutine = True
            break  # at least one component has not yet finished
    
    # refresh the screen
    if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
        win.flip()

# -------Ending Routine "valence"-------
for thisComponent in valenceComponents:
    if hasattr(thisComponent, "setAutoDraw"):
        thisComponent.setAutoDraw(False)
# check responses
if valencekey.keys in ['', [], None]:  # No response was made
    valencekey.keys=None
thisExp.addData('valencekey.keys',valencekey.keys)
if valencekey.keys != None:  # we had a response
    thisExp.addData('valencekey.rt', valencekey.rt)
thisExp.nextEntry()
# the Routine "valence" was not non-slip safe, so reset the non-slip timer
routineTimer.reset()

# set up handler to look after randomisation of conditions etc
pracwords = data.TrialHandler(nReps=1, method='random', 
    extraInfo=expInfo, originPath=-1,
    trialList=data.importConditions('EMOTIONWORDS.xlsx'),
    seed=None, name='pracwords')
thisExp.addLoop(pracwords)  # add the loop to the experiment
thisPracword = pracwords.trialList[0]  # so we can initialise stimuli with some values
# abbreviate parameter names if possible (e.g. rgb = thisPracword.rgb)
if thisPracword != None:
    for paramName in thisPracword:
        exec('{} = thisPracword[paramName]'.format(paramName))

for thisPracword in pracwords:
    currentLoop = pracwords
    # abbreviate parameter names if possible (e.g. rgb = thisPracword.rgb)
    if thisPracword != None:
        for paramName in thisPracword:
            exec('{} = thisPracword[paramName]'.format(paramName))
    
    # ------Prepare to start Routine "Differentiation"-------
    t = 0
    DifferentiationClock.reset()  # clock
    frameN = -1
    continueRoutine = True
    # update component parameters for each repeat
    WordInstruction.setText(Instruction



)
    WordResponse = event.BuilderKeyResponse()
    EmoWord.setText(Word)
    # keep track of which components have finished
    DifferentiationComponents = [WordInstruction, WordResponse, WordScale, EmoWord]
    for thisComponent in DifferentiationComponents:
        if hasattr(thisComponent, 'status'):
            thisComponent.status = NOT_STARTED
    
    # -------Start Routine "Differentiation"-------
    while continueRoutine:
        # get current time
        t = DifferentiationClock.getTime()
        frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
        # update/draw components on each frame
        
        # *WordInstruction* updates
        if t >= 0.0 and WordInstruction.status == NOT_STARTED:
            # keep track of start time/frame for later
            WordInstruction.tStart = t
            WordInstruction.frameNStart = frameN  # exact frame index
            WordInstruction.setAutoDraw(True)
        
        # *WordResponse* updates
        if t >= 0.0 and WordResponse.status == NOT_STARTED:
            # keep track of start time/frame for later
            WordResponse.tStart = t
            WordResponse.frameNStart = frameN  # exact frame index
            WordResponse.status = STARTED
            # keyboard checking is just starting
            win.callOnFlip(WordResponse.clock.reset)  # t=0 on next screen flip
            event.clearEvents(eventType='keyboard')
        if WordResponse.status == STARTED:
            theseKeys = event.getKeys(keyList=['1', '2', '3', '4'])
            
            # check for quit:
            if "escape" in theseKeys:
                endExpNow = True
            if len(theseKeys) > 0:  # at least one key was pressed
                WordResponse.keys = theseKeys[-1]  # just the last key pressed
                WordResponse.rt = WordResponse.clock.getTime()
                # a response ends the routine
                continueRoutine = False
        
        # *WordScale* updates
        if t >= 0.0 and WordScale.status == NOT_STARTED:
            # keep track of start time/frame for later
            WordScale.tStart = t
            WordScale.frameNStart = frameN  # exact frame index
            WordScale.setAutoDraw(True)
        
        # *EmoWord* updates
        if t >= 0.0 and EmoWord.status == NOT_STARTED:
            # keep track of start time/frame for later
            EmoWord.tStart = t
            EmoWord.frameNStart = frameN  # exact frame index
            EmoWord.setAutoDraw(True)
        frameRemains = 0.0 + 1.0- win.monitorFramePeriod * 0.75  # most of one frame period left
        if EmoWord.status == STARTED and t >= frameRemains:
            EmoWord.setAutoDraw(False)
        
        # check for quit (typically the Esc key)
        if endExpNow or event.getKeys(keyList=["escape"]):
            core.quit()
        
        # check if all components have finished
        if not continueRoutine:  # a component has requested a forced-end of Routine
            break
        continueRoutine = False  # will revert to True if at least one component still running
        for thisComponent in DifferentiationComponents:
            if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                continueRoutine = True
                break  # at least one component has not yet finished
        
        # refresh the screen
        if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
            win.flip()
    
    # -------Ending Routine "Differentiation"-------
    for thisComponent in DifferentiationComponents:
        if hasattr(thisComponent, "setAutoDraw"):
            thisComponent.setAutoDraw(False)
    # check responses
    if WordResponse.keys in ['', [], None]:  # No response was made
        WordResponse.keys=None
    pracwords.addData('WordResponse.keys',WordResponse.keys)
    if WordResponse.keys != None:  # we had a response
        pracwords.addData('WordResponse.rt', WordResponse.rt)
    # the Routine "Differentiation" was not non-slip safe, so reset the non-slip timer
    routineTimer.reset()
    thisExp.nextEntry()
    
# completed 1 repeats of 'pracwords'

# get names of stimulus parameters
if pracwords.trialList in ([], [None], None):
    params = []
else:
    params = pracwords.trialList[0].keys()
# save data for this loop
pracwords.saveAsText(filename + 'pracwords.csv', delim=',',
    stimOut=params,
    dataOut=['n','all_mean','all_std', 'all_raw'])

# ------Prepare to start Routine "beginstudy"-------
t = 0
beginstudyClock.reset()  # clock
frameN = -1
continueRoutine = True
# update component parameters for each repeat
advance = event.BuilderKeyResponse()
# keep track of which components have finished
beginstudyComponents = [text, advance]
for thisComponent in beginstudyComponents:
    if hasattr(thisComponent, 'status'):
        thisComponent.status = NOT_STARTED

# -------Start Routine "beginstudy"-------
while continueRoutine:
    # get current time
    t = beginstudyClock.getTime()
    frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
    # update/draw components on each frame
    
    # *text* updates
    if t >= 0.0 and text.status == NOT_STARTED:
        # keep track of start time/frame for later
        text.tStart = t
        text.frameNStart = frameN  # exact frame index
        text.setAutoDraw(True)
    
    # *advance* updates
    if t >= 0.0 and advance.status == NOT_STARTED:
        # keep track of start time/frame for later
        advance.tStart = t
        advance.frameNStart = frameN  # exact frame index
        advance.status = STARTED
        # keyboard checking is just starting
        event.clearEvents(eventType='keyboard')
    if advance.status == STARTED:
        theseKeys = event.getKeys()
        
        # check for quit:
        if "escape" in theseKeys:
            endExpNow = True
        if len(theseKeys) > 0:  # at least one key was pressed
            # a response ends the routine
            continueRoutine = False
    
    # check for quit (typically the Esc key)
    if endExpNow or event.getKeys(keyList=["escape"]):
        core.quit()
    
    # check if all components have finished
    if not continueRoutine:  # a component has requested a forced-end of Routine
        break
    continueRoutine = False  # will revert to True if at least one component still running
    for thisComponent in beginstudyComponents:
        if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
            continueRoutine = True
            break  # at least one component has not yet finished
    
    # refresh the screen
    if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
        win.flip()

# -------Ending Routine "beginstudy"-------
for thisComponent in beginstudyComponents:
    if hasattr(thisComponent, "setAutoDraw"):
        thisComponent.setAutoDraw(False)
# the Routine "beginstudy" was not non-slip safe, so reset the non-slip timer
routineTimer.reset()

# set up handler to look after randomisation of conditions etc
songtrials = data.TrialHandler(nReps=1, method='random', 
    extraInfo=expInfo, originPath=-1,
    trialList=data.importConditions('Music.xlsx'),
    seed=None, name='songtrials')
thisExp.addLoop(songtrials)  # add the loop to the experiment
thisSongtrial = songtrials.trialList[0]  # so we can initialise stimuli with some values
# abbreviate parameter names if possible (e.g. rgb = thisSongtrial.rgb)
if thisSongtrial != None:
    for paramName in thisSongtrial:
        exec('{} = thisSongtrial[paramName]'.format(paramName))

for thisSongtrial in songtrials:
    currentLoop = songtrials
    # abbreviate parameter names if possible (e.g. rgb = thisSongtrial.rgb)
    if thisSongtrial != None:
        for paramName in thisSongtrial:
            exec('{} = thisSongtrial[paramName]'.format(paramName))
    
    # ------Prepare to start Routine "music"-------
    t = 0
    musicClock.reset()  # clock
    frameN = -1
    continueRoutine = True
    # update component parameters for each repeat
    playsong.setSound(song)
    playsong.setVolume(2, log=False)
    port.write('marker'.encode())
    # keep track of which components have finished
    musicComponents = [playsong]
    for thisComponent in musicComponents:
        if hasattr(thisComponent, 'status'):
            thisComponent.status = NOT_STARTED
    
    # -------Start Routine "music"-------
    while continueRoutine:
        # get current time
        t = musicClock.getTime()
        frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
        # update/draw components on each frame
        # start/stop playsong
        if t >= 0.0 and playsong.status == NOT_STARTED:
            # keep track of start time/frame for later
            playsong.tStart = t
            playsong.frameNStart = frameN  # exact frame index
            win.callOnFlip(playsong.play)  # screen flip
        
        
        # check for quit (typically the Esc key)
        if endExpNow or event.getKeys(keyList=["escape"]):
            core.quit()
        
        # check if all components have finished
        if not continueRoutine:  # a component has requested a forced-end of Routine
            break
        continueRoutine = False  # will revert to True if at least one component still running
        for thisComponent in musicComponents:
            if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                continueRoutine = True
                break  # at least one component has not yet finished
        
        # refresh the screen
        if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
            win.flip()
    
    # -------Ending Routine "music"-------
    for thisComponent in musicComponents:
        if hasattr(thisComponent, "setAutoDraw"):
            thisComponent.setAutoDraw(False)
    playsong.stop()  # ensure sound has stopped at end of routine
    port.write("00". encode())
    # the Routine "music" was not non-slip safe, so reset the non-slip timer
    routineTimer.reset()
    
    # ------Prepare to start Routine "arousal"-------
    t = 0
    arousalClock.reset()  # clock
    frameN = -1
    continueRoutine = True
    # update component parameters for each repeat
    arousalkey = event.BuilderKeyResponse()
    # keep track of which components have finished
    arousalComponents = [arousaltext, arousalkey]
    for thisComponent in arousalComponents:
        if hasattr(thisComponent, 'status'):
            thisComponent.status = NOT_STARTED
    
    # -------Start Routine "arousal"-------
    while continueRoutine:
        # get current time
        t = arousalClock.getTime()
        frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
        # update/draw components on each frame
        
        # *arousaltext* updates
        if t >= 0.0 and arousaltext.status == NOT_STARTED:
            # keep track of start time/frame for later
            arousaltext.tStart = t
            arousaltext.frameNStart = frameN  # exact frame index
            arousaltext.setAutoDraw(True)
        
        # *arousalkey* updates
        if t >= 0.0 and arousalkey.status == NOT_STARTED:
            # keep track of start time/frame for later
            arousalkey.tStart = t
            arousalkey.frameNStart = frameN  # exact frame index
            arousalkey.status = STARTED
            # keyboard checking is just starting
            win.callOnFlip(arousalkey.clock.reset)  # t=0 on next screen flip
            event.clearEvents(eventType='keyboard')
        if arousalkey.status == STARTED:
            theseKeys = event.getKeys(keyList=['1', '2', '3', '4', '5'])
            
            # check for quit:
            if "escape" in theseKeys:
                endExpNow = True
            if len(theseKeys) > 0:  # at least one key was pressed
                arousalkey.keys = theseKeys[-1]  # just the last key pressed
                arousalkey.rt = arousalkey.clock.getTime()
                # a response ends the routine
                continueRoutine = False
        
        # check for quit (typically the Esc key)
        if endExpNow or event.getKeys(keyList=["escape"]):
            core.quit()
        
        # check if all components have finished
        if not continueRoutine:  # a component has requested a forced-end of Routine
            break
        continueRoutine = False  # will revert to True if at least one component still running
        for thisComponent in arousalComponents:
            if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                continueRoutine = True
                break  # at least one component has not yet finished
        
        # refresh the screen
        if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
            win.flip()
    
    # -------Ending Routine "arousal"-------
    for thisComponent in arousalComponents:
        if hasattr(thisComponent, "setAutoDraw"):
            thisComponent.setAutoDraw(False)
    # check responses
    if arousalkey.keys in ['', [], None]:  # No response was made
        arousalkey.keys=None
    songtrials.addData('arousalkey.keys',arousalkey.keys)
    if arousalkey.keys != None:  # we had a response
        songtrials.addData('arousalkey.rt', arousalkey.rt)
    # the Routine "arousal" was not non-slip safe, so reset the non-slip timer
    routineTimer.reset()
    
    # ------Prepare to start Routine "valence"-------
    t = 0
    valenceClock.reset()  # clock
    frameN = -1
    continueRoutine = True
    # update component parameters for each repeat
    valencekey = event.BuilderKeyResponse()
    # keep track of which components have finished
    valenceComponents = [valencetext, valencekey]
    for thisComponent in valenceComponents:
        if hasattr(thisComponent, 'status'):
            thisComponent.status = NOT_STARTED
    
    # -------Start Routine "valence"-------
    while continueRoutine:
        # get current time
        t = valenceClock.getTime()
        frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
        # update/draw components on each frame
        
        # *valencetext* updates
        if t >= 0.0 and valencetext.status == NOT_STARTED:
            # keep track of start time/frame for later
            valencetext.tStart = t
            valencetext.frameNStart = frameN  # exact frame index
            valencetext.setAutoDraw(True)
        
        # *valencekey* updates
        if t >= 0.0 and valencekey.status == NOT_STARTED:
            # keep track of start time/frame for later
            valencekey.tStart = t
            valencekey.frameNStart = frameN  # exact frame index
            valencekey.status = STARTED
            # keyboard checking is just starting
            win.callOnFlip(valencekey.clock.reset)  # t=0 on next screen flip
            event.clearEvents(eventType='keyboard')
        if valencekey.status == STARTED:
            theseKeys = event.getKeys(keyList=['1', '2', '3', '4', '5'])
            
            # check for quit:
            if "escape" in theseKeys:
                endExpNow = True
            if len(theseKeys) > 0:  # at least one key was pressed
                valencekey.keys = theseKeys[-1]  # just the last key pressed
                valencekey.rt = valencekey.clock.getTime()
                # a response ends the routine
                continueRoutine = False
        
        # check for quit (typically the Esc key)
        if endExpNow or event.getKeys(keyList=["escape"]):
            core.quit()
        
        # check if all components have finished
        if not continueRoutine:  # a component has requested a forced-end of Routine
            break
        continueRoutine = False  # will revert to True if at least one component still running
        for thisComponent in valenceComponents:
            if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                continueRoutine = True
                break  # at least one component has not yet finished
        
        # refresh the screen
        if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
            win.flip()
    
    # -------Ending Routine "valence"-------
    for thisComponent in valenceComponents:
        if hasattr(thisComponent, "setAutoDraw"):
            thisComponent.setAutoDraw(False)
    # check responses
    if valencekey.keys in ['', [], None]:  # No response was made
        valencekey.keys=None
    songtrials.addData('valencekey.keys',valencekey.keys)
    if valencekey.keys != None:  # we had a response
        songtrials.addData('valencekey.rt', valencekey.rt)
    # the Routine "valence" was not non-slip safe, so reset the non-slip timer
    routineTimer.reset()
    
    # set up handler to look after randomisation of conditions etc
    words = data.TrialHandler(nReps=1, method='random', 
        extraInfo=expInfo, originPath=-1,
        trialList=data.importConditions('EMOTIONWORDS.xlsx'),
        seed=None, name='words')
    thisExp.addLoop(words)  # add the loop to the experiment
    thisWord = words.trialList[0]  # so we can initialise stimuli with some values
    # abbreviate parameter names if possible (e.g. rgb = thisWord.rgb)
    if thisWord != None:
        for paramName in thisWord:
            exec('{} = thisWord[paramName]'.format(paramName))
    
    for thisWord in words:
        currentLoop = words
        # abbreviate parameter names if possible (e.g. rgb = thisWord.rgb)
        if thisWord != None:
            for paramName in thisWord:
                exec('{} = thisWord[paramName]'.format(paramName))
        
        # ------Prepare to start Routine "Differentiation"-------
        t = 0
        DifferentiationClock.reset()  # clock
        frameN = -1
        continueRoutine = True
        # update component parameters for each repeat
        WordInstruction.setText(Instruction



)
        WordResponse = event.BuilderKeyResponse()
        EmoWord.setText(Word)
        # keep track of which components have finished
        DifferentiationComponents = [WordInstruction, WordResponse, WordScale, EmoWord]
        for thisComponent in DifferentiationComponents:
            if hasattr(thisComponent, 'status'):
                thisComponent.status = NOT_STARTED
        
        # -------Start Routine "Differentiation"-------
        while continueRoutine:
            # get current time
            t = DifferentiationClock.getTime()
            frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
            # update/draw components on each frame
            
            # *WordInstruction* updates
            if t >= 0.0 and WordInstruction.status == NOT_STARTED:
                # keep track of start time/frame for later
                WordInstruction.tStart = t
                WordInstruction.frameNStart = frameN  # exact frame index
                WordInstruction.setAutoDraw(True)
            
            # *WordResponse* updates
            if t >= 0.0 and WordResponse.status == NOT_STARTED:
                # keep track of start time/frame for later
                WordResponse.tStart = t
                WordResponse.frameNStart = frameN  # exact frame index
                WordResponse.status = STARTED
                # keyboard checking is just starting
                win.callOnFlip(WordResponse.clock.reset)  # t=0 on next screen flip
                event.clearEvents(eventType='keyboard')
            if WordResponse.status == STARTED:
                theseKeys = event.getKeys(keyList=['1', '2', '3', '4'])
                
                # check for quit:
                if "escape" in theseKeys:
                    endExpNow = True
                if len(theseKeys) > 0:  # at least one key was pressed
                    WordResponse.keys = theseKeys[-1]  # just the last key pressed
                    WordResponse.rt = WordResponse.clock.getTime()
                    # a response ends the routine
                    continueRoutine = False
            
            # *WordScale* updates
            if t >= 0.0 and WordScale.status == NOT_STARTED:
                # keep track of start time/frame for later
                WordScale.tStart = t
                WordScale.frameNStart = frameN  # exact frame index
                WordScale.setAutoDraw(True)
            
            # *EmoWord* updates
            if t >= 0.0 and EmoWord.status == NOT_STARTED:
                # keep track of start time/frame for later
                EmoWord.tStart = t
                EmoWord.frameNStart = frameN  # exact frame index
                EmoWord.setAutoDraw(True)
            frameRemains = 0.0 + 1.0- win.monitorFramePeriod * 0.75  # most of one frame period left
            if EmoWord.status == STARTED and t >= frameRemains:
                EmoWord.setAutoDraw(False)
            
            # check for quit (typically the Esc key)
            if endExpNow or event.getKeys(keyList=["escape"]):
                core.quit()
            
            # check if all components have finished
            if not continueRoutine:  # a component has requested a forced-end of Routine
                break
            continueRoutine = False  # will revert to True if at least one component still running
            for thisComponent in DifferentiationComponents:
                if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                    continueRoutine = True
                    break  # at least one component has not yet finished
            
            # refresh the screen
            if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
                win.flip()
        
        # -------Ending Routine "Differentiation"-------
        for thisComponent in DifferentiationComponents:
            if hasattr(thisComponent, "setAutoDraw"):
                thisComponent.setAutoDraw(False)
        # check responses
        if WordResponse.keys in ['', [], None]:  # No response was made
            WordResponse.keys=None
        words.addData('WordResponse.keys',WordResponse.keys)
        if WordResponse.keys != None:  # we had a response
            words.addData('WordResponse.rt', WordResponse.rt)
        # the Routine "Differentiation" was not non-slip safe, so reset the non-slip timer
        routineTimer.reset()
        thisExp.nextEntry()
        
    # completed 1 repeats of 'words'
    
    # get names of stimulus parameters
    if words.trialList in ([], [None], None):
        params = []
    else:
        params = words.trialList[0].keys()
    # save data for this loop
    words.saveAsText(filename + 'words.csv', delim=',',
        stimOut=params,
        dataOut=['n','all_mean','all_std', 'all_raw'])
    thisExp.nextEntry()
    
# completed 1 repeats of 'songtrials'

# get names of stimulus parameters
if songtrials.trialList in ([], [None], None):
    params = []
else:
    params = songtrials.trialList[0].keys()
# save data for this loop
songtrials.saveAsText(filename + 'songtrials.csv', delim=',',
    stimOut=params,
    dataOut=['n','all_mean','all_std', 'all_raw'])

# ------Prepare to start Routine "end"-------
t = 0
endClock.reset()  # clock
frameN = -1
continueRoutine = True
# update component parameters for each repeat
key_resp_2 = event.BuilderKeyResponse()
# keep track of which components have finished
endComponents = [MusicEnd, key_resp_2]
for thisComponent in endComponents:
    if hasattr(thisComponent, 'status'):
        thisComponent.status = NOT_STARTED

# -------Start Routine "end"-------
while continueRoutine:
    # get current time
    t = endClock.getTime()
    frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
    # update/draw components on each frame
    
    # *MusicEnd* updates
    if t >= 0.0 and MusicEnd.status == NOT_STARTED:
        # keep track of start time/frame for later
        MusicEnd.tStart = t
        MusicEnd.frameNStart = frameN  # exact frame index
        MusicEnd.setAutoDraw(True)
    
    # *key_resp_2* updates
    if t >= 0.0 and key_resp_2.status == NOT_STARTED:
        # keep track of start time/frame for later
        key_resp_2.tStart = t
        key_resp_2.frameNStart = frameN  # exact frame index
        key_resp_2.status = STARTED
        # keyboard checking is just starting
        win.callOnFlip(key_resp_2.clock.reset)  # t=0 on next screen flip
        event.clearEvents(eventType='keyboard')
    if key_resp_2.status == STARTED:
        theseKeys = event.getKeys(keyList=['y', 'n', 'left', 'right', 'space'])
        
        # check for quit:
        if "escape" in theseKeys:
            endExpNow = True
        if len(theseKeys) > 0:  # at least one key was pressed
            key_resp_2.keys = theseKeys[-1]  # just the last key pressed
            key_resp_2.rt = key_resp_2.clock.getTime()
            # a response ends the routine
            continueRoutine = False
    
    # check for quit (typically the Esc key)
    if endExpNow or event.getKeys(keyList=["escape"]):
        core.quit()
    
    # check if all components have finished
    if not continueRoutine:  # a component has requested a forced-end of Routine
        break
    continueRoutine = False  # will revert to True if at least one component still running
    for thisComponent in endComponents:
        if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
            continueRoutine = True
            break  # at least one component has not yet finished
    
    # refresh the screen
    if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
        win.flip()

# -------Ending Routine "end"-------
for thisComponent in endComponents:
    if hasattr(thisComponent, "setAutoDraw"):
        thisComponent.setAutoDraw(False)
# check responses
if key_resp_2.keys in ['', [], None]:  # No response was made
    key_resp_2.keys=None
thisExp.addData('key_resp_2.keys',key_resp_2.keys)
if key_resp_2.keys != None:  # we had a response
    thisExp.addData('key_resp_2.rt', key_resp_2.rt)
thisExp.nextEntry()
# the Routine "end" was not non-slip safe, so reset the non-slip timer
routineTimer.reset()
ser.close()

# these shouldn't be strictly necessary (should auto-save)
thisExp.saveAsWideText(filename+'.csv')
thisExp.saveAsPickle(filename)
logging.flush()
# make sure everything is closed down
thisExp.abort()  # or data files will save again on exit
win.close()
core.quit()
