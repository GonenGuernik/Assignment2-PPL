// ========================================================
// Value type definition for L4

import { isPrimOp, CExp, PrimOp, VarDecl, Binding } from './L3-ast';
import { Env, makeEmptyEnv } from './L3-env-env';
import { append } from 'ramda';
import { isArray, isNumber, isString } from '../shared/type-predicates';
import { valueToLitExp } from './L3-eval-sub';
import { map } from "ramda";


export type Value = SExpValue;

export type Functional = PrimOp | Closure;
export const isFunctional = (x: any): x is Functional => isPrimOp(x) || isClosure(x);

// ========================================================
// Closure for L4 - the field env is added.
// We also use a frame-based representation of closures as opposed to one env per var.
export type Closure = {
    tag: "Closure";
    params: VarDecl[];
    body: CExp[];
    env: Env;
}

export type ClassVal = {
    tag: "ClassVal";
    fields: VarDecl[];
    methods: Binding[];
    env: Env;
}

export type ObjectVal = {
    tag: "ObjectVal";
    classVal: ClassVal;
    // fields: VarDecl[];
    values: Value[];
    bindings: Binding[];
    // methods: Binding[];
    env: Env;
}

export const makeClass = (fields: VarDecl[], methods: Binding[]): ClassVal =>
    ({tag: "ClassVal", fields: fields, methods: methods, env : makeEmptyEnv()});
export const makeClassEnv = (fields: VarDecl[], methods: Binding[], env: Env): ClassVal =>
    ({tag: "ClassVal", fields: fields, methods: methods, env: env});
export const isClass = (x: any): x is ClassVal => x.tag === "ClassVal";

export const makeObject = (classVal: ClassVal, values: Value[]): ObjectVal => 
    ({tag: "ObjectVal", classVal: classVal, values: values, env : makeEmptyEnv()});
    const vars = map((v: VarDecl) => v.var, classVal.fields);
    const litVals = map(valueToLitExp, values); 
    bindings = map((val: CExp[]) => zipWith(makeBinding, vars, val), values);


    const vars = map(b => b[0], bindings);
    const valsResult = mapResult(parseL3CExp, map(second, bindings));
    const bindingsResult = mapv(valsResult, (vals: CExp[]) => zipWith(makeBinding, vars, vals));


const applyClosure = (proc: Closure, args: Value[], env: Env): Result<Value> => {
    const vars = map((v: VarDecl) => v.var, proc.params);
    const body = renameExps(proc.body);
    const litArgs : CExp[] = map(valueToLitExp, args);
    return evalSequence(substitute(body, vars, litArgs), env);

export const makeObjectEnv = (classVal: ClassVal, values: Value[], env: Env): ObjectVal =>
    ({tag: "ObjectVal", classVal: classVal, values: values, env: env});
export const isObject = (x: any): x is Object => x.tag === "ObjectVal";

export const makeClosure = (params: VarDecl[], body: CExp[]): Closure =>
    ({tag: "Closure", params: params, body: body, env : makeEmptyEnv()});
export const makeClosureEnv = (params: VarDecl[], body: CExp[], env: Env): Closure =>
    ({tag: "Closure", params: params, body: body, env: env});
export const isClosure = (x: any): x is Closure => x.tag === "Closure";



// ========================================================
// SExp
export type CompoundSExp = {
    tag: "CompoundSexp";
    val1: SExpValue;
    val2: SExpValue;
}
export type EmptySExp = {
    tag: "EmptySExp";
}
export type SymbolSExp = {
    tag: "SymbolSExp";
    val: string;
}

export type SExpValue = number | boolean | string | PrimOp | Closure | SymbolSExp | EmptySExp | CompoundSExp | ClassVal | ObjectVal;
export const isSExp = (x: any): x is SExpValue =>
    typeof(x) === 'string' || typeof(x) === 'boolean' || typeof(x) === 'number' ||
    isSymbolSExp(x) || isCompoundSExp(x) || isEmptySExp(x) || isPrimOp(x) || isClosure(x);

export const makeCompoundSExp = (val1: SExpValue, val2: SExpValue): CompoundSExp =>
    ({tag: "CompoundSexp", val1: val1, val2 : val2});
export const isCompoundSExp = (x: any): x is CompoundSExp => x.tag === "CompoundSexp";

export const makeEmptySExp = (): EmptySExp => ({tag: "EmptySExp"});
export const isEmptySExp = (x: any): x is EmptySExp => x.tag === "EmptySExp";

export const makeSymbolSExp = (val: string): SymbolSExp =>
    ({tag: "SymbolSExp", val: val});
export const isSymbolSExp = (x: any): x is SymbolSExp => x.tag === "SymbolSExp";

// LitSExp are equivalent to JSON - they can be parsed and read as literal values
// like SExp except that non functional values (PrimOp and Closures) can be embedded at any level.
export type LitSExp = number | boolean | string | SymbolSExp | EmptySExp | CompoundSExp;

// Printable form for values
export const closureToString = (c: Closure): string =>
    // `<Closure ${c.params} ${L3unparse(c.body)}>`
    `<Closure ${c.params} ${c.body}>`

export const compoundSExpToArray = (cs: CompoundSExp, res: string[]): string[] | { s1: string[], s2: string } =>
    isEmptySExp(cs.val2) ? append(valueToString(cs.val1), res) :
    isCompoundSExp(cs.val2) ? compoundSExpToArray(cs.val2, append(valueToString(cs.val1), res)) :
    ({ s1: append(valueToString(cs.val1), res), s2: valueToString(cs.val2)})
 
export const compoundSExpToString = (cs: CompoundSExp, css = compoundSExpToArray(cs, [])): string => 
    isArray(css) ? `(${css.join(' ')})` :
    `(${css.s1.join(' ')} . ${css.s2})`

export const valueToString = (val: Value): string =>
    isNumber(val) ?  val.toString() :
    val === true ? '#t' :
    val === false ? '#f' :
    isString(val) ? `"${val}"` :
    isClosure(val) ? closureToString(val) :
    isPrimOp(val) ? val.op :
    isSymbolSExp(val) ? val.val :
    isEmptySExp(val) ? "'()" :
    isCompoundSExp(val) ? compoundSExpToString(val) :
    isClass(val) ? classToString(val) :
    isObject(val) ? objectToString(val) :
    val;
