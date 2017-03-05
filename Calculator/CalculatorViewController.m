//
//  CalculatorViewController.m
//  Calculator
//
//  Created by Guillaume Lasnier on 30/12/11.
//  Copyright (c) 2011 Prima Solutions. All rights reserved.
//

#import "CalculatorViewController.h"
#import "CalculatorBrain.h"

@interface CalculatorViewController ()
@property (nonatomic) BOOL userIsInTheMiddleOfEnteringANumber;
@property (nonatomic,strong) CalculatorBrain *brain;
@property (nonatomic,strong) NSDictionary *testVariableValues;
@end

@implementation CalculatorViewController

@synthesize display = _display;
@synthesize input = _input;
@synthesize variablesDisplay = _variablesDisplay;
@synthesize userIsInTheMiddleOfEnteringANumber = _userIsInTheMiddleOfEnteringANumber;
@synthesize brain = _brain;
@synthesize testVariableValues = _testVariableValues;


-(CalculatorBrain *) brain {
    if (!_brain) {
        _brain = [[CalculatorBrain alloc] init];
    }
    return _brain;
}

#pragma User input

- (void)updateDisplay {
    // Update user input
    self.input.text = [CalculatorBrain descriptionOfProgram:self.brain.program];
}

- (void)appendDigitOrPeriod:(NSString *)digitOrPeriod {
    if (self.userIsInTheMiddleOfEnteringANumber) {
        self.display.text = [self.display.text stringByAppendingString:digitOrPeriod];
    }
    else {
        self.display.text = digitOrPeriod;
        self.userIsInTheMiddleOfEnteringANumber = YES;
    }
}

- (IBAction)digitPressed:(UIButton *)sender {
    [self appendDigitOrPeriod:sender.currentTitle];
}

- (IBAction)periodPressed {
    NSRange range = [self.display.text rangeOfString:@"."];
    if (range.location == NSNotFound) {
        [self appendDigitOrPeriod:@"."];
    }
    
}

- (IBAction)clearPressed {
    [self.brain clear];
    self.display.text = @"0";
    self.input.text = @"";
    self.userIsInTheMiddleOfEnteringANumber = NO;
}

- (IBAction)undoPressed {
    if (self.userIsInTheMiddleOfEnteringANumber) {
        
    } else {
        [self.brain pop];
    }
    [self updateDisplay];
}

#pragma Test methods

- (void) test {
    self.display.text = [NSString stringWithFormat:@"%g", [[CalculatorBrain class] runProgram:self.brain.program usingVariableValues:self.testVariableValues]];
    NSString *variablesText = @"";
    for (id variable in [[CalculatorBrain class] variablesUsedInProgram:self.brain.program]) {
        NSNumber *value = [self.testVariableValues objectForKey:variable];
        NSString *format = @"  %@ = %g";
        if ([variablesText length] == 0) {
            format = @"%@ = %g";
        }
        variablesText = [variablesText stringByAppendingFormat:format, variable, value.doubleValue];
    }
    self.variablesDisplay.text = variablesText;
}

- (IBAction)testOnePressed {
    self.testVariableValues = [NSDictionary dictionaryWithObjectsAndKeys:[NSNumber numberWithDouble:0.5],@"a", [NSNumber numberWithDouble:2.3333], @"b", [NSNumber numberWithDouble:M_PI], @"x",  nil];
    [self test];
}

- (IBAction)testTwoPressed {
    self.testVariableValues = [NSDictionary dictionaryWithObjectsAndKeys:[NSNumber numberWithDouble:-1324.16666], @"a", [NSNumber numberWithDouble:1.2345], @"b", [NSNumber numberWithDouble:1234], @"x", nil];
    [self test];
}

- (IBAction)testThreePressed:(id)sender {
    self.testVariableValues = [NSDictionary dictionaryWithObjectsAndKeys:[NSNumber numberWithDouble:0.5], @"a", [NSNumber numberWithDouble:7/3], @"b", [NSNumber numberWithDouble:1234], @"x", nil];
    [self test];
}

#pragma CalculatorBrain interactions

- (IBAction)enterPressed {
    [self.brain pushOperand:[self.display.text doubleValue]];
    // Editing ended
    self.userIsInTheMiddleOfEnteringANumber = NO;
    [self updateDisplay];
}

- (IBAction)operationOrVariablePressed:(UIButton *)sender {
    if (self.userIsInTheMiddleOfEnteringANumber) {
        [self enterPressed];
    }
    NSString *operation = sender.currentTitle;
    [self.brain pushOperationOrVariable:operation];
    [self updateDisplay];
}


- (void)viewDidUnload {
    self.variablesDisplay = nil;
    [super viewDidUnload];
}

@end
