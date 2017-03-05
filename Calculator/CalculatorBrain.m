//
//  CalculatorBrain.m
//  Calculator
//
//  Created by Guillaume Lasnier on 30/12/11.
//  Copyright (c) 2011 Prima Solutions. All rights reserved.
//

#import "CalculatorBrain.h"


@interface CalculatorBrain()
@property (nonatomic, strong) NSMutableArray *programStack;
@end

@implementation CalculatorBrain

@synthesize programStack = _programStack;

- (NSMutableArray *)programStack {
    if (_programStack == nil) _programStack = [[NSMutableArray alloc] init];
    return _programStack;
}

- (id)program {
    return [self.programStack copy];
}


+ (id)popStack:(NSMutableArray *)stack {
    id topOfStack = [stack lastObject];
    if (topOfStack) [stack removeLastObject];
    return topOfStack;
}

- (id) pop {
    return [[CalculatorBrain class] popStack:self.programStack];
}

- (void)pushOperand:(double) operand {
    [self.programStack addObject:[NSNumber numberWithDouble:operand]];
}

- (void)pushOperationOrVariable:(NSString *)operation {
    [self.programStack addObject:operation];
}

- (void)clear {
    [self.programStack removeAllObjects];
}

+ (BOOL)isTwoOperandOperation:(id)obj {
    return [obj isKindOfClass:[NSString class]] && [[NSSet setWithObjects:@"+", @"*", @"-", @"/", nil] containsObject:obj];
}

+ (BOOL)isSingleOperandOperation:(id)obj {
    return [obj isKindOfClass:[NSString class]] && [[NSSet setWithObjects:@"cos", @"sin", @"sqrt",  nil] containsObject:obj];
}

+ (BOOL)isNoOperandOperation:(id)obj {
    return [@"π" isEqualToString:obj];
}

+ (BOOL)isOperation:(id)obj {
    return [[CalculatorBrain class] isSingleOperandOperation:obj] || [[CalculatorBrain class] isTwoOperandOperation:obj] || [[CalculatorBrain class] isNoOperandOperation:obj];
}

+ (BOOL)isVariable:(id)obj {
    return [obj isKindOfClass:[NSString class]] && ![[CalculatorBrain class] isOperation:obj];
}

+ (NSSet *)variablesUsedInProgram:(id)program {
    NSSet * result;
    if ([program isKindOfClass:[NSArray class]]) {
        NSArray *stack = program;
        NSIndexSet * variablesIndex = [stack indexesOfObjectsPassingTest:^(id obj, NSUInteger idx, BOOL *stop) {
            return [[CalculatorBrain class] isVariable:obj];
        }];
        result = [NSSet setWithArray:[stack objectsAtIndexes:variablesIndex]];
    }
    return result;
}

+ (double)popOffProgramStack:(NSMutableArray *)stack usingVariableValues:(NSDictionary *)variableValues{
    double result = 0;
    
    id topOfStack;
    topOfStack = [self popStack:stack];
    
    if ([topOfStack isKindOfClass:[NSNumber class]]) {
        result = [topOfStack doubleValue];
    }
    else if ([[CalculatorBrain class] isOperation:topOfStack]) {
        NSString *operation = topOfStack;
        if ([operation isEqualToString:@"+"]) {
            result = [self popOffProgramStack:stack usingVariableValues:variableValues] + [self popOffProgramStack:stack usingVariableValues:variableValues];
        } else if ([operation isEqualToString:@"*"]) {
            result = [self popOffProgramStack:stack usingVariableValues:variableValues] * [self popOffProgramStack:stack usingVariableValues:variableValues];
        } else if ([operation isEqualToString:@"-"]) {
            double subtrahend = [self popOffProgramStack:stack usingVariableValues:variableValues];
            result = [self popOffProgramStack:stack usingVariableValues:variableValues] - subtrahend;
        } else if ([operation isEqualToString:@"/"]) {
            double divisor = [self popOffProgramStack:stack usingVariableValues:variableValues];
            if (divisor) {
                result = [self popOffProgramStack:stack usingVariableValues:variableValues] / divisor;
            }
        } else if ([operation isEqualToString:@"π"]) {
            result = M_PI;
        } else if ([operation isEqualToString:@"cos"]) {
            result = cos([self popOffProgramStack:stack usingVariableValues:variableValues]);
        } else if ([operation isEqualToString:@"sin"]) {
            result = sin([self popOffProgramStack:stack usingVariableValues:variableValues]);
        } else if ([operation isEqualToString:@"sqrt"]) {
            double square = [self popOffProgramStack:stack usingVariableValues:variableValues];
            if (square >= 0) {
                result = sqrt(square);
            }
        }
    } else {
        NSString *variableName = topOfStack;
        result = [[variableValues objectForKey:variableName] doubleValue];    
    }
    
    return result;
}

+ (double)runProgram:(id)program usingVariableValues:(NSDictionary *)variableValues {
    NSMutableArray *stack;
    if ([program isKindOfClass:[NSArray class]]) {
        stack = [program mutableCopy];
    }
    return [self popOffProgramStack:stack usingVariableValues:variableValues];
}

+ (double)runProgram:(id)program {
    return [self runProgram:program usingVariableValues:nil];
}

+ (NSString *) removeUnnecessaryParentheses:(NSString *) aString {
    NSString *result = aString;
    if ([aString hasPrefix:@"("] && [aString hasSuffix:@")"]) {
        result = [[aString substringFromIndex:1] substringToIndex:[result length] - 2];
    }
    return result;
}

+ (NSString *) descriptionOfTopOfStack:(NSMutableArray *)stack  {
    NSString *result;
    id topOfStack = [stack lastObject];
    if (topOfStack) [stack removeLastObject];
    if ([topOfStack isKindOfClass:[NSNumber class]]) {
        result = [NSString stringWithFormat:@"%g", [topOfStack doubleValue]];
    } else if ([[CalculatorBrain class] isSingleOperandOperation:topOfStack]) {
        result = [[CalculatorBrain class] descriptionOfTopOfStack:stack];
        result = [[CalculatorBrain class] removeUnnecessaryParentheses:result];
        result = [NSString stringWithFormat:@"%@(%@)", topOfStack, result];
    } else if ([[CalculatorBrain class] isTwoOperandOperation:topOfStack]) {
        NSString *right = [[CalculatorBrain class] descriptionOfTopOfStack:stack];
        result = [NSString stringWithFormat:@"%@ %@ %@", [[CalculatorBrain class] descriptionOfTopOfStack:stack], topOfStack, right];
        if ([topOfStack isEqualToString:@"+"] || [topOfStack isEqualToString:@"-"]) {
            result = [NSString stringWithFormat:@"(%@)", result]; 
        }
    } else {
        result = topOfStack;
    }
    
    return result;
}

+ (NSString *) descriptionOfProgram:(id)program {
    NSString *result;
    if ([program isKindOfClass:[NSArray class]]) {
        NSMutableArray *stack = [program mutableCopy];
        result = [[CalculatorBrain class] descriptionOfTopOfStack:stack];
        result = [[CalculatorBrain class] removeUnnecessaryParentheses:result];
        if ([stack count] > 0) {
             result = [[[CalculatorBrain class] descriptionOfProgram:stack] stringByAppendingFormat:@", %@", result];
        }
    } else {
        result = [NSString stringWithFormat:@"%@ is not a valid program", program];
    }
             
    return result;
}

@end
