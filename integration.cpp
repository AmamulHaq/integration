#include <iostream>
#include <vector>
#include <stack>
#include <string>
#include <cctype>
#include <sstream>
#include <cmath>
#include <algorithm>
#include <map>

using namespace std;

struct Token {
    string value;
    bool isOperator;
    bool isVariable;
    bool isNumber;

    Token(string val, bool isOp, bool isVar, bool isNum)
        : value(val), isOperator(isOp), isVariable(isVar), isNumber(isNum) {}
};

vector<Token> tokenize(const string& expr) {
    vector<Token> tokens;
    string current;
    bool expectUnary = true;

    for (size_t i = 0; i < expr.size();) {
        if (isspace(expr[i])) { ++i; continue; }

        if (expectUnary && (expr[i] == '+' || expr[i] == '-')) {
            string op = (expr[i] == '-') ? "u-" : "u+";
            tokens.emplace_back(op, true, false, false);
            ++i;
            expectUnary = false;
            continue;
        }

        if (isdigit(expr[i]) || expr[i] == '.') {
            current += expr[i++];
            while (i < expr.size() && (isdigit(expr[i]) || expr[i] == '.')) {
                current += expr[i++];
            }
            tokens.emplace_back(current, false, false, true);
            current.clear();
            expectUnary = false;
            continue;
        }

        if (isalpha(expr[i])) {
            tokens.emplace_back(string(1, expr[i]), false, true, false);
            ++i;
            expectUnary = false;
            continue;
        }

        if (expr[i] == '^' || expr[i] == '*' || expr[i] == '/' ||
            expr[i] == '+' || expr[i] == '-' || expr[i] == '(' || expr[i] == ')') {
            tokens.emplace_back(string(1, expr[i]), true, false, false);
            expectUnary = (expr[i] == '(' || expr[i] == '+' || expr[i] == '-' ||
                           expr[i] == '*' || expr[i] == '/' || expr[i] == '^');
            ++i;
            continue;
        }

        ++i;
    }

    return tokens;
}

vector<Token> infixToPostfix(const vector<Token>& tokens) {
    vector<Token> output;
    stack<Token> opStack;

    auto precedence = [](const string& op) {
        if (op == "u-" || op == "u+") return 5;
        if (op == "^") return 4;
        if (op == "*" || op == "/") return 3;
        if (op == "+" || op == "-") return 2;
        return 0;
    };

    auto isRightAssoc = [](const string& op) { return op == "^"; };

    for (const Token& token : tokens) {
        if (!token.isOperator) {
            output.push_back(token);
        } else if (token.value == "(") {
            opStack.push(token);
        } else if (token.value == ")") {
            while (!opStack.empty() && opStack.top().value != "(") {
                output.push_back(opStack.top());
                opStack.pop();
            }
            if (!opStack.empty()) opStack.pop();
        } else {
            while (!opStack.empty() && opStack.top().value != "(" &&
                   (precedence(opStack.top().value) > precedence(token.value) ||
                    (precedence(opStack.top().value) == precedence(token.value) &&
                     !isRightAssoc(token.value)))) {
                output.push_back(opStack.top());
                opStack.pop();
            }
            opStack.push(token);
        }
    }

    while (!opStack.empty()) {
        output.push_back(opStack.top());
        opStack.pop();
    }
    return output;
}

string formatNumber(double num) {
    ostringstream oss;
    oss.precision(6);
    oss << fixed << num;
    string result = oss.str();
    result.erase(result.find_last_not_of('0') + 1, string::npos);
    if (result.back() == '.') result.pop_back();
    return result;
}

vector<vector<Token>> splitInfixTerms(const vector<Token>& tokens) {
    vector<vector<Token>> terms;
    vector<Token> currentTerm;
    vector<Token> currentSign;

    for (size_t i = 0; i < tokens.size(); ++i) {
        const Token& token = tokens[i];
        if (token.value == "+" || token.value == "-") {
            if (!currentTerm.empty()) {
                currentTerm.insert(currentTerm.begin(), currentSign.begin(), currentSign.end());
                terms.push_back(currentTerm);
                currentTerm.clear();
                currentSign.clear();
            }
            if (token.value == "-") {
                currentSign = {Token("u-", true, false, false)};
            } else {
                currentSign.clear();
            }
        } else {
            currentTerm.push_back(token);
        }
    }

    if (!currentTerm.empty()) {
        currentTerm.insert(currentTerm.begin(), currentSign.begin(), currentSign.end());
        terms.push_back(currentTerm);
    }

    return terms;
}

vector<Token> integrateTerm(const vector<Token>& termPostfix, char var) {
    stack<double> coeffStack;
    stack<double> expStack;
    stack<bool> varStack;

    for (const Token& token : termPostfix) {
        if (token.isNumber) {
            coeffStack.push(stod(token.value));
            expStack.push(0.0);
            varStack.push(false);
        } else if (token.isVariable && token.value[0] == var) {
            coeffStack.push(1.0);
            expStack.push(1.0);
            varStack.push(true);
        } else if (token.value == "u-") {
            if (coeffStack.empty()) return {Token("0", false, false, true)};
            double coeff = coeffStack.top(); coeffStack.pop();
            coeffStack.push(-coeff);
        } else if (token.value == "u+") {
            continue;
        } else if (token.isOperator) {
            if (token.value == "*") {
                double c2 = coeffStack.top(); coeffStack.pop();
                double e2 = expStack.top(); expStack.pop();
                bool v2 = varStack.top(); varStack.pop();

                double c1 = coeffStack.top(); coeffStack.pop();
                double e1 = expStack.top(); expStack.pop();
                bool v1 = varStack.top(); varStack.pop();

                coeffStack.push(c1 * c2);
                expStack.push(e1 + e2);
                varStack.push(v1 || v2);
            } else if (token.value == "^") {
                double exponent = coeffStack.top(); coeffStack.pop();
                expStack.pop();
                varStack.pop();
                double base = coeffStack.top(); coeffStack.pop();
                double exp = expStack.top(); expStack.pop();
                bool var = varStack.top(); varStack.pop();

                coeffStack.push(base);
                expStack.push(exponent);
                varStack.push(true);
            } else if (token.value == "/") {
                double denominator = coeffStack.top(); coeffStack.pop();
                double eDenominator = expStack.top(); expStack.pop();
                bool vDenominator = varStack.top(); varStack.pop();

                double numerator = coeffStack.top(); coeffStack.pop();
                double eNumerator = expStack.top(); expStack.pop();
                bool vNumerator = varStack.top(); varStack.pop();

                coeffStack.push(numerator / denominator);
                expStack.push(eNumerator);
                varStack.push(vNumerator);
            }
        }
    }

    if (coeffStack.empty()) return {Token("0", false, false, true)};
    double coeff = coeffStack.top();
    double exponent = expStack.top();
    bool hasVar = varStack.top();

    if (!hasVar) {
        return {Token(formatNumber(coeff) + "*" + string(1, var), false, false, false)};
    }

    double newExp = exponent + 1.0;
    double newCoeff = coeff / newExp;
    vector<Token> result;

    result.emplace_back(formatNumber(newCoeff), false, false, true);
    result.emplace_back("*", true, false, false);
    result.emplace_back("(", true, false, false);
    result.emplace_back(string(1, var), false, true, false);
    result.emplace_back("^", true, false, false);
    result.emplace_back(formatNumber(newExp), false, false, true);
    result.emplace_back(")", true, false, false);

    return result;
}

string tokensToString(const vector<Token>& tokens) {
    stringstream ss;
    for (size_t i = 0; i < tokens.size(); ++i) {
        if (i > 0 && tokens[i].isNumber && tokens[i - 1].isVariable)
            ss << "*";
        ss << tokens[i].value;
    }
    return ss.str();
}

vector<Token> integrateExpression(const vector<Token>& infixTokens, char var) {
    vector<vector<Token>> terms = splitInfixTerms(infixTokens);
    vector<Token> result;

    for (size_t i = 0; i < terms.size(); ++i) {
        bool isNegative = !terms[i].empty() && terms[i][0].value == "u-";
        vector<Token> termTokens = terms[i];
        if (isNegative) {
            termTokens = vector<Token>(terms[i].begin() + 1, terms[i].end());
        }

        vector<Token> postfix = infixToPostfix(termTokens);
        vector<Token> integrated = integrateTerm(postfix, var);

        if (integrated.empty()) continue;

        if (isNegative) {
            result.emplace_back("-", true, false, false);
        } else if (!result.empty()) {
            result.emplace_back("+", true, false, false);
        }

        result.insert(result.end(), integrated.begin(), integrated.end());
    }

    if (result.empty()) {
        return {Token("0", false, false, true)};
    }

    return result;
}

int main() {
    cout << "Enter algebraic expression: ";
    string expr;
    getline(cin, expr);

    char var = 'x';
    vector<Token> tokens = tokenize(expr);
    vector<Token> integral = integrateExpression(tokens, var);

    cout << "\nResult: ∫(" << expr << ") d" << var << " = " 
         << tokensToString(integral) << " + C" << endl;

    return 0;
}

/*
Enter algebraic expression: 6*x
Result: ∫(6*x) dx = 3*(x^2) + C

Enter algebraic expression: 6*(x^1/2)
Result: ∫(6*(x^1/2)) dx = 1.5*(x^2) + C

Enter algebraic expression: 2*(x^3)-6*x+7
Result: ∫(2*(x^3)-6*x+7) dx = 0.5*(x^4)-3*(x^2)+7*x + C

Enter algebraic expression: 2*x^4-5*x+3
Result: ∫(2*x^4-5*x+3) dx = 0.4*(x^5)-2.5*(x^2)+3*x + C

Enter algebraic expression: 4*x^5-2*x^3+x-8
Result: ∫(4*x^5-2*x^3+x-8) dx = 0.67*(x^6)-0.5*(x^4)+0.5*(x^2)-8*x+C


*/