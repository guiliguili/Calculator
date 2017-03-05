//
//  CalculatorViewController.h
//  Calculator
//
//  Created by Guillaume Lasnier on 30/12/11.
//  Copyright (c) 2011 Prima Solutions. All rights reserved.
//

#import <UIKit/UIKit.h>

@interface CalculatorViewController : UIViewController

@property (weak, nonatomic) IBOutlet UILabel *display;
@property (weak, nonatomic) IBOutlet UILabel *input;
@property (weak, nonatomic) IBOutlet UILabel *variablesDisplay;

@end
