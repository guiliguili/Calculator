//
//  CalculatorBrain.h
//  Calculator
//
//  Created by Guillaume Lasnier on 30/12/11.
//  Copyright (c) 2011 Prima Solutions. All rights reserved.
//

#import <Foundation/Foundation.h>

@interface CalculatorBrain : NSObject

-(void)pushOperand:(double) operand;
-(void)pushOperationOrVariable:(NSString *)operation;
-(id)pop;
-(void)clear;

@property (nonatomic, readonly) id program;

+(NSString *)descriptionOfProgram:(id)program;
+(double)runProgram:(id)program;
+(double)runProgram:(id)program usingVariableValues:(NSDictionary *)variableValues;
+(NSSet *)variablesUsedInProgram:(id)program;

@end
