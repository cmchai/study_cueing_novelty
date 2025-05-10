var consentText = ['<div class = "InstrBx"><h1>Welcome to the experiment</h1>'+
                   '<p>This is a research project conducted at Ghent University. The data we collect during the experiment are not linked to any potentially identifying information. The data will be used \
                   solely for research purposes. Anonymized data from the study will be registered and archived at a trusted public data repository, in line with current data sharing practices. You are \
                   free to stop the experiment by closing your browser window at any time, which will not be of any disadvantage to you. By clicking "I agree", you affirm that you are at least 18 years of \
                   age, and understand the nature of your participation in this research. If you do not wish to participate, please close this window.<p></div>'];
var mainInstruction = [
                       //////////////////////////////////// the first page //////////////////////////////////////////
                       '<div class = "InstrBx"><h1>Hello, welcome to the experiment!</h1>' +
                       '<p>Today you will be playing a game which tests how quickly and accurately you can learn and perform new tasks.</p>' +
                      
                       '<p>At the start of each trial, you will see the task cue combing three rules on the screen, followed immediately by another two words on the screen. For example:</p>' +

                       '<p><img src="maininstruction_v3_CueStimImage.jpg" width="720" height="160"></img></p>' +

                       '<p>With regards to the three rules, the above-presented word on the screen specifies what <b>relation</b> you will have to evaluate. This rule can be one of the following\
                        4 options: BOTH, AT LEAST ONE, FIRST, SECOND.</p>' +

                       '<p>The middle-presented word on the screen indicates which <b>feature</b> you will have to evaluate. Similarly, there are 4 possible features: GREEN, SOFT, LOUD, SWEET.</p>' + 

                       '<p>The below-presented word indicates which <b>finger</b> you need to use to respond, and it can be one of your four fingers: LEFT MIDDLE, LEFT INDEX, RIGHT INDEX, RIGHT MIDDLE.</p>' +

                       '<p>In the example given above, your task is to judge whether <b>BOTH</b> of mint and lemonade are <b>GREEN</b> in color. If they are <b>BOTH GREEN</b>, then you need to respond using your\
                        <b>LEFT INDEX</b> finger. Otherwise, you need to press the <b>Spacebar</b> instead.</p>',
                       
                       //////////////////////////////////// the second page /////////////////////////////////////////
                       '<p>Two words will appear on the screen after the task instruction, which are \"mint\" and \"lemonade\" in this case. mint is usually GREEN in color, but lemonade is usually not GREEN. \
                       Therefore, they are <b>not</b> BOTH GREEN. Thus, you should press the Spacebar in this case.</p>'+

                       '<p>You can perform the task efficiently by placing your left middle and index fingers on the letter \"D\" and \"F\" respectively, and placing your right middle and index fingers\
                        on \"K\" and \"J\", and your thumbs on the Spacebar.</p>' +

                       '<p><img src="keyboard.png" width="600" height="200"></img></p>',

                       //////////////////////////////////// the third page //////////////////////////////////////////

                       '<div class = "InstrBx"><p>In terms of each <b>feature</b>, such as GREEN, the <b>relation</b> rules can be one of the following four rules, and should be understood as follows:</p>' + 

                       '<p> &#8226 BOTH: Are both objects green?' +
                       '<br> &#8226 AT LEAST ONE: Is at least one of 2 objects green (one is green, or both are green)?' +
                       '<br> &#8226 FIRST: Is the first object (on the upper screen) green?' +
                       '<br> &#8226 SECOND: Is the second object (on the lower screen) green?' +

                       '<p>Please judge the <b>features</b> (GREEN, SOFT, LOUD, SWEET) based on common sense. For example, if your task is evaluating SOFT, the words that you will see should be \
                       evaluated on whether they are <b>typically or usually</b> considered SOFT. The answers are based on how most people would evaluate them. Thus, do not overthink this.</p>' + 

                       '<p>The whole experiment contains <b>two parts</b>. The first part is the training session, wherein you will learn to perform 4 particular tasks. Each task rule combinations\
                        one by one. In the second part, you will be performing both the tasks that you already practiced (<b>practiced tasks</b>) during the previous training session, as well as new tasks\
                         that you have not seen before (<b>novel tasks</b>).</p>' +

                       '<p>Now you can press the Next button to start the first practice block. Good luck!</p></div>'
                       ];

var testMainInstruction = ['<p>Congratulations! You just finished the first part of the experiment, you are more than half-way done!</p>'+
                           '<p>For the second part of the experiment, you will be performing both <b>practiced tasks</b> (tasks that you have performed before) and <b>novel tasks</b> (tasks that you\
                            have never performed before). There are four blocks in total. In some blocks, we will give you some extra help. Specifically, in those blocks you will see a <b>task type cue</b>, \
                            indicating whether a practiced task or a novel task will occur, such as the following:</p>'+
                                                     
                           '<p>* novel task *</p>'+
                           
                           '<p><b>Please use this cue to prepare for the upcoming task as much as possible!</b></p>'+ 

                           '<p>In other blocks, the cues are not informative at all. In this case, you do not need to do any preparation, the cues will look like the following:</p>'+

                           '<p>* xxxxx xxxx *</p>' +

                           '<p>The rest of the trial will be exactly the same as the first part of the experiment. Please keep in mind that, from now on, there will be no feedback telling \
                           your response is correct or not anymore.</p>' +

                           '<p>Please press the Next button to start your first testing block.</p>'];

var pracBlockInstruction = ['<p>Next, there will be 60 word pairs appearing on the screen pair after pair. It will be your task to evaluate whether each pair of words fulfill the above rule and\
                             respond accordingly. There will be feedback after each trial telling you if you respond correctly or not. Please act <b>as fast and accurately as possible</b>.' +

                            '<p>Keep in mind that the task rules <b>will NOT</b> appear throughout the whole block, so please remember the task now and keep it in mind when you go through the block.</p>' +
    
                            '<p>Press the Spacebar to start the block. you will see the task rules briefly again. Good luck!</p>'];

var testCueInstruction = ['<p>For this block, you will be performing the task with a task type cue, such as the following:</p>'+
                          '<p>* practiced task *</p>' +  
                          '<p>or:</p>' +
                          '<p>* novel task *</p>' +
                          '<p>As you can see, the cue will inform you if the upcoming task is a total novel task or a task that you have practiced before. <b>Please use this cue to prepare for the upcoming task!</b></p>' +
                          '<p>Press the Spacebar to start the block. Good luck!</p>'];

var testNoCueInstruction = ['<p>For this block, you will be performing the task with an un-informative task type cue, such as the following:</p>'+
                            '<p>* xxxxxx xxxx *</p>'+
                            '<p>Since this cue does not give you any information about the next task, you do NOT need to do any preparation.</p>'+
                            '<p>Press the Spacebar to start the block. Good luck!</p>']

var endMessage = ['<p><div class = "InstrBx">Congratulations! now you successfully finished the experiment now!</p>'+ 
                  '<p>Thanks again for your participation!</p>'+
                  '<p>Feel free the contact the experimenter (Mengqiao.Chai@ugent.be) if you want to have more information about this experiment</p>'+
                  '<p>Now you can press NEXT to go back to Prolific</p></div>'];