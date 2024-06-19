import { ClassExp, ProcExp, Exp, Program, makeVarDecl, makeVarRef, makeIfExp, makeAppExp, makePrimOp, makeLitExp, makeProcExp, isClassExp, isDefineExp, makeDefineExp, makeProgram, isBoolExp, isNumExp, isStrExp, isLitExp, isVarRef, isProcExp, isIfExp, isAppExp, isPrimOp, isLetExp, isProgram, CExp, makeLetExp, makeBinding, makeClassExp, isCExp, isExp, isAtomicExp, IfExp, makeBoolExp } from "./L3-ast";
import { Result, bind, makeFailure, makeOk, mapResult } from "../shared/result";
import { map, reduce, reduceRight } from "ramda";
import { makeSymbolSExp } from "./L3-value";
import { first, second, rest, allT, isEmpty, isNonEmptyList, List } from "../shared/list";
import {  } from "../shared/list";

/*
Purpose: Transform ClassExp to ProcExp
Signature: class2proc(classExp)
Type: ClassExp => ProcExp
*/
export const class2proc = (exp: ClassExp): ProcExp => {
    const fields = exp.fields;
    const methods = exp.methods;

    const msgVar = makeVarDecl('msg');
    const msgRef = makeVarRef('msg');

    const methodCases = map((method) => {
        const methodName = method.var.var;
        const methodBody = method.val;

        return makeIfExp(
            makeAppExp(makePrimOp('eq?'), [makeVarRef('msg'), makeLitExp(makeSymbolSExp(methodName))]),
            // (methodBody),
            // makeAppExp(makeProcExp([], [methodBody]), []), // Enclose lambda in parentheses,
            makeAppExp(methodBody, []),
            makeBoolExp(false) // Use BoolExp for false case
            // makeLitExp(makeSymbolSExp('#f'))
        );
    }, methods);

    // const nestedIf = reduceRight((ifExp, nextIf) => makeIfExp(ifExp.test, ifExp.then, nextIf), [] as IfExp[], methodCases);
    // const nestedIf = reduceRight((ifExp, nextIf) => makeIfExp(ifExp.test, ifExp.then, nextIf), methodCases[0], methodCases.slice(1));
    // const nestedIf = reduce((acc, methodCase) => makeIfExp(methodCase.test, methodCase.then, acc), makeLitExp(makeSymbolSExp('#f')) as CExp, methodCases.reverse());
    const nestedIf = reduce(
        (acc, methodCase) => makeIfExp(methodCase.test, methodCase.then, acc),
        makeBoolExp(false) as CExp, // Ensure initial accumulator is of type CExp
        methodCases.reverse()
    );

    return makeProcExp(fields, [makeProcExp([msgVar], [nestedIf])]);
};
  
/*
Purpose: Transform all class forms in the given AST to procs
Signature: lexTransform(AST)
Type: [Exp | Program] => Result<Exp | Program>
*/
// export const rewriteAllLet = (exp: Program | Exp): Program | Exp =>
//     isExp(exp) ? transformExp(exp) :
//     isProgram(exp) ? makeProgram(map(transformExp, exp.exps)) :
//     exp;


// const transformExp = (exp: Exp): Exp =>
//     isCExp(exp) ? transformCExp(exp) :
//     isDefineExp(exp) ? makeDefineExp(exp.var, transformCExp(exp.val)) :
//     exp;


// const transformCExp = (exp: CExp): CExp =>
//     isAtomicExp(exp) ? exp :
//     isLitExp(exp) ? exp :
//     isIfExp(exp) ? makeIfExp(transformCExp(exp.test),
//                              transformCExp(exp.then),
//                              transformCExp(exp.alt)) :
//     isAppExp(exp) ? makeAppExp(transformCExp(exp.rator),
//                                map(transformCExp, exp.rands)) :
//     isProcExp(exp) ? makeProcExp(exp.args, map(transformCExp, exp.body)) :
//     isLetExp(exp) ? transformCExp(class2proc(exp)) :
//     exp;

export const lexTransform = (exp: Exp | Program): Result<Exp | Program> => { 
    const transformExp = (exp: Exp): Exp =>
        isCExp(exp) ? transformCExp(exp) :
        isDefineExp(exp) ? makeDefineExp(exp.var, transformCExp(exp.val)) :
        exp;
    
    const transformCExp = (cexp: CExp): CExp => 
            isAtomicExp(cexp) ? cexp :
            isLitExp(cexp) ? cexp :
            isProcExp(cexp) ? makeProcExp(cexp.args, map(transformCExp, cexp.body)) :
            isIfExp(cexp) ? makeIfExp(transformCExp(cexp.test), transformCExp(cexp.then), transformCExp(cexp.alt)) :
            isAppExp(cexp) ? makeAppExp(transformCExp(cexp.rator), map(transformCExp, cexp.rands)) :
            isLetExp(cexp) ? makeLetExp(map(binding => makeBinding(binding.var.var, transformCExp(binding.val)), cexp.bindings), map(transformCExp, cexp.body)) :
            isClassExp(cexp) ? class2proc(makeClassExp(cexp.fields, map(method => makeBinding(method.var.var, transformCExp(method.val)), cexp.methods))) :
            cexp;

    return isExp(exp) ? makeOk(transformExp(exp)) :
    isProgram(exp) ? makeOk(makeProgram(map(transformExp, exp.exps))) :
    exp;


    // if (isDefineExp(exp)) {
    //     // check if need to makeDefineExp
    //     return makeOk(makeDefineExp(exp.var, transformCExp(exp.val)));
    // } else if (isProgram(exp)) {
    //     const transformedExps = map(lexTransform, exp.exps);
    //     // handle program exp.
    //     return bind(transformedExps, (exps: Exp[]) => makeOk(makeProgram(exps)));
    // } else if (isCExp(exp)) {
    //     return makeOk(transformCExp(exp));
    // } else {
    //     makeFailure("Unrecognized expression.")
    // }
};

    // old code - might be useless
    //     if (isClassExp(exp)) {
    //         return class2proc(exp);
    //     } else if (isDefineExp(exp)) {
    //         const transformedVal = transformExp(exp.val);
    //         return makeDefineExp(exp.var, transformedVal);
    //     } else {
    //         return exp;
    //     }
    // };

    // if (isDefineExp(exp) || isClassExp(exp)) {
    //     return makeOk(transformExp(exp));
    // } else if (exp.tag === "Program") {
    //     const transformedExps = mapResult(transformExp, exp.exps);
    //     return bind(transformedExps, (exps: Exp[]) => makeOk(makeProgram(exps)));
    // } else {
    //     return makeOk(exp);
    // }
