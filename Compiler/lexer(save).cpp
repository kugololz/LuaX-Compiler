#include <iostream>
#include <string>
#include <vector>
#include <cctype>
#include <unordered_map>
#include <fstream>

// 1. Enum de Tokens
enum class Token {
    LOCAL,
    IDENTIFIER,
    STRING_LITERAL,
    EQUALS,
    NUMBER,
    PLUS,
    WHILE,
    DO,
    FOR,
    IN,
    COMMA,
    MINUS,
    MULTIPLY,
    DIVIDE,
    LPAREN,
    PRINT,
    RPAREN,
    IF,
    THEN,
    ELSE,
    END,
    EQ,      // '=='
    NEQ,     // '~='
    LT,      // '<'
    LTE,     // '<='
    GT,      // '>'
    GTE,     // '>='
    END_OF_FILE,
    ERROR
};

struct TokenValue {
    Token type;
    std::string value;
};

enum class DataType {
    INTEGER,
    STRING
};

struct Variable {
    DataType type;
    int int_value;
    std::string string_value;
    bool is_local;
};

struct Value {
    DataType type;
    int int_value;
    std::string string_value;
};

class Lexer {
public:
    Lexer(const std::string &src) : src(src), index(0) {}

    TokenValue next_token() {
        skip_whitespace_and_comments();

        if (index >= src.length()) {
            return {Token::END_OF_FILE, ""};
        }

        char current = src[index];

        // Reconocer literales de cadena
        if (current == '"') {
            index++; // Saltar la comilla inicial
            std::string value;
            while (index < src.length() && src[index] != '"') {
                if (src[index] == '\\') {
                    // Manejar caracteres de escape
                    index++;
                    if (index < src.length()) {
                        char escaped_char = src[index];
                        switch (escaped_char) {
                            case 'n': value += '\n'; break;
                            case 't': value += '\t'; break;
                            case '"': value += '"'; break;
                            case '\\': value += '\\'; break;
                            // Agrega más caracteres de escape si lo deseas
                            default: value += escaped_char; break;
                        }
                        index++;
                    }
                } else {
                    value += src[index];
                    index++;
                }
            }
            if (index >= src.length() || src[index] != '"') {
                std::cerr << "Error: Cadena sin cerrar." << std::endl;
                return {Token::END_OF_FILE, ""};
            }
            index++; // Saltar la comilla final
            return {Token::STRING_LITERAL, value};
        }

        if (current == 'p' && src.substr(index, 5) == "print") {
            index += 5;
            return {Token::PRINT, "print"};
        }

        if (current == ',') {
            index++;
            return {Token::COMMA, ","};
        }

        if (current == 'w' && src.substr(index, 5) == "while") {
            index += 5;
            return {Token::WHILE, "while"};
        }

        if (current == 'd' && src.substr(index, 2) == "do") {
            index += 2;
            return {Token::DO, "do"};
        }

        if (current == 'f' && src.substr(index, 3) == "for") {
            index += 3;
            return {Token::FOR, "for"};
        }

        if (current == 'i' && src.substr(index, 2) == "in") {
            index += 2;
            return {Token::IN, "in"};
        }

        // Reconocer palabras clave y operadores
        if (current == 'l' && src.substr(index, 5) == "local") {
            index += 5;
            return {Token::LOCAL, "local"};
        }

        if (current == 'i' && src.substr(index, 2) == "if") {
            index += 2;
            return {Token::IF, "if"};
        }

        if (current == 't' && src.substr(index, 4) == "then") {
            index += 4;
            return {Token::THEN, "then"};
        }

        if (current == 'e' && src.substr(index, 4) == "else") {
            index += 4;
            return {Token::ELSE, "else"};
        }

        if (current == 'e' && src.substr(index, 3) == "end") {
            index += 3;
            return {Token::END, "end"};
        }

        // Reconocer operadores de comparación
        if (current == '=' && index + 1 < src.length() && src[index + 1] == '=') {
            index += 2;
            return {Token::EQ, "=="};
        }

        if (current == '~' && index + 1 < src.length() && src[index + 1] == '=') {
            index += 2;
            return {Token::NEQ, "~="};
        }

        if (current == '<') {
            if (index + 1 < src.length() && src[index + 1] == '=') {
                index += 2;
                return {Token::LTE, "<="};
            } else {
                index++;
                return {Token::LT, "<"};
            }
        }

        if (current == '>') {
            if (index + 1 < src.length() && src[index + 1] == '=') {
                index += 2;
                return {Token::GTE, ">="};
            } else {
                index++;
                return {Token::GT, ">"};
            }
        }

        if (isalpha(current)) {
            std::string id;
            while (index < src.length() && (isalnum(src[index]) || src[index] == '_')) {
                id += src[index++];
            }
            return {Token::IDENTIFIER, id};
        }

        if (current == '=') {
            index++;
            return {Token::EQUALS, "="};
        }

        if (current == '+') {
            index++;
            return {Token::PLUS, "+"};
        }

        // Reconocer '-'
        if (current == '-') {
            index++;
            return {Token::MINUS, "-"};
        }

        // Reconocer '*'
        if (current == '*') {
            index++;
            return {Token::MULTIPLY, "*"};
        }

        // Reconocer '/'
        if (current == '/') {
            index++;
            return {Token::DIVIDE, "/"};
        }

        // Reconocer '('
        if (current == '(') {
            index++;
            return {Token::LPAREN, "("};
        }

        // Reconocer ')'
        if (current == ')') {
            index++;
            return {Token::RPAREN, ")"};
        }

        if (isdigit(current)) {
            std::string num;
            while (index < src.length() && isdigit(src[index])) {
                num += src[index++];
            }
            return {Token::NUMBER, num};
        }

        return {Token::ERROR, std::string(1, current)};
    }

private:
    void skip_whitespace_and_comments() {
        while (index < src.length()) {
            if (std::isspace(src[index])) {
                index++;
            } else if (src[index] == '-' && index + 1 < src.length() && src[index + 1] == '-') {
                // Es un comentario, saltar hasta el final de la línea
                index += 2; // Saltar los dos guiones
                while (index < src.length() && src[index] != '\n') {
                    index++;
                }
            } else {
                break;
            }
        }
    }

    std::string src;
    size_t index;
};

class SymbolTable {
public:
    void add_variable(const std::string& name, DataType type, bool is_local = false) {
        if (table.find(name) != table.end()) {
            table[name].type = type;
            table[name].is_local = is_local;
        } else {
            Variable var;
            var.type = type;
            var.is_local = is_local;
            table[name] = var;
        }
    }

    void set_value(const std::string& name, int value) {
        if (table.find(name) != table.end()) {
            table[name].type = DataType::INTEGER;
            table[name].int_value = value;
        }
    }

    void set_value(const std::string& name, const std::string& value) {
        if (table.find(name) != table.end()) {
            table[name].type = DataType::STRING;
            table[name].string_value = value;
        }
    }

    void set_value(const std::string& name, const Value& value) {
        if (table.find(name) != table.end()) {
            table[name].type = value.type;
            table[name].int_value = value.int_value;
            table[name].string_value = value.string_value;
        }
    }

    bool exists(const std::string& name) const {
        return table.find(name) != table.end();
    }

    Variable get_variable(const std::string& name) const {
        return table.at(name);
    }

    void print_table() const {
        std::cout << "Symbol Table:" << std::endl;
        for (const auto& pair : table) {
            std::cout << pair.first << " = ";
            if (pair.second.type == DataType::INTEGER) {
                std::cout << pair.second.int_value;
            } else if (pair.second.type == DataType::STRING) {
                std::cout << "\"" << pair.second.string_value << "\"";
            }
            std::cout << " (" << (pair.second.is_local ? "local" : "global") << ")" << std::endl;
        }
    }

private:
    std::unordered_map<std::string, Variable> table;
};

// 6. OpCode
enum class OpCode {
    PUSH_VAR,
    ADD,
    SUB,
    MUL,
    PRINT,
    DIV,
    STORE,
    EQ_OP,
    NEQ_OP,
    PUSH_INT,
    PUSH_STRING,
    LT_OP,
    LTE_OP,
    GT_OP,
    GTE_OP,
    JMP,           // Salto incondicional
    JMP_IF_FALSE   // Salto si la cima de la pila es falsa (0)
};

class Instruction {
public:
    OpCode op;
    int operand_int;
    std::string operand_string;
    std::string variable;
    bool is_local;

    // Constructor para PUSH_INT
    Instruction(OpCode op, int value) : op(op), operand_int(value), is_local(false) {}

    // Constructor para PUSH_STRING (añadimos un dummy int para distinguir)
    Instruction(OpCode op, const std::string& value, int dummy) : op(op), operand_string(value), is_local(false) {}

    // Constructor para operaciones con variables
    Instruction(OpCode op, const std::string& var_name, bool is_local)
        : op(op), variable(var_name), is_local(is_local) {}

    // Constructor para instrucciones sin operandos
    Instruction(OpCode op) : op(op), operand_int(0), is_local(false) {}

    std::string to_string() const {
        switch (op) {
            case OpCode::PUSH_INT:
                return "PUSH_INT " + std::to_string(operand_int);
            case OpCode::PUSH_STRING:
                return "PUSH_STRING \"" + operand_string + "\"";
            case OpCode::PUSH_VAR:
                return "PUSH_VAR " + variable;
            case OpCode::STORE:
                return "STORE " + variable + (is_local ? " (local)" : " (global)");
            case OpCode::ADD:
                return "ADD";
            case OpCode::SUB:
                return "SUB";
            case OpCode::MUL:
                return "MUL";
            case OpCode::DIV:
                return "DIV";
            case OpCode::PRINT:
                return "PRINT";
            case OpCode::EQ_OP:
                return "EQ_OP";
            case OpCode::NEQ_OP:
                return "NEQ_OP";
            case OpCode::LT_OP:
                return "LT_OP";
            case OpCode::LTE_OP:
                return "LTE_OP";
            case OpCode::GT_OP:
                return "GT_OP";
            case OpCode::GTE_OP:
                return "GTE_OP";
            case OpCode::JMP:
                return "JMP " + std::to_string(operand_int);
            case OpCode::JMP_IF_FALSE:
                return "JMP_IF_FALSE " + std::to_string(operand_int);
            default:
                return "UNKNOWN";
        }
    }
};

void execute_instructions(const std::vector<Instruction>& instructions, SymbolTable& symbolTable) {
    std::vector<Value> stack;
    size_t pc = 0;
    while (pc < instructions.size()) {
        const auto& instr = instructions[pc];
        switch (instr.op) {
            case OpCode::PUSH_INT: {
                Value val;
                val.type = DataType::INTEGER;
                val.int_value = instr.operand_int;
                stack.push_back(val);
                break;
            }
            case OpCode::NEQ_OP: {
                if (stack.size() < 2) {
                    std::cerr << "Error: No hay suficientes valores en la pila para NEQ_OP." << std::endl;
                    break;
                }
                Value b = stack.back(); stack.pop_back();
                Value a = stack.back(); stack.pop_back();
                Value result;
                result.type = DataType::INTEGER;
                result.int_value = (a.int_value != b.int_value) ? 1 : 0;
                stack.push_back(result);
                break;
            }

            case OpCode::LT_OP: {
                if (stack.size() < 2) {
                    std::cerr << "Error: No hay suficientes valores en la pila para LT_OP." << std::endl;
                    break;
                }
                Value b = stack.back(); stack.pop_back();
                Value a = stack.back(); stack.pop_back();
                Value result;
                result.type = DataType::INTEGER;
                result.int_value = (a.int_value < b.int_value) ? 1 : 0;
                stack.push_back(result);
                break;
            }

            case OpCode::LTE_OP: {
                if (stack.size() < 2) {
                    std::cerr << "Error: No hay suficientes valores en la pila para LTE_OP." << std::endl;
                    break;
                }
                Value b = stack.back(); stack.pop_back();
                Value a = stack.back(); stack.pop_back();
                Value result;
                result.type = DataType::INTEGER;
                result.int_value = (a.int_value <= b.int_value) ? 1 : 0;
                stack.push_back(result);
                break;
            }

            case OpCode::GT_OP: {
                if (stack.size() < 2) {
                    std::cerr << "Error: No hay suficientes valores en la pila para GT_OP." << std::endl;
                    break;
                }
                Value b = stack.back(); stack.pop_back();
                Value a = stack.back(); stack.pop_back();
                Value result;
                result.type = DataType::INTEGER;
                result.int_value = (a.int_value > b.int_value) ? 1 : 0;
                stack.push_back(result);
                break;
            }

            case OpCode::GTE_OP: {
                if (stack.size() < 2) {
                    std::cerr << "Error: No hay suficientes valores en la pila para GTE_OP." << std::endl;
                    break;
                }
                Value b = stack.back(); stack.pop_back();
                Value a = stack.back(); stack.pop_back();
                Value result;
                result.type = DataType::INTEGER;
                result.int_value = (a.int_value >= b.int_value) ? 1 : 0;
                stack.push_back(result);
                break;
            }

            case OpCode::PUSH_STRING: {
                Value val;
                val.type = DataType::STRING;
                val.string_value = instr.operand_string;
                stack.push_back(val);
                break;
            }
           case OpCode::PUSH_VAR: {
                if (symbolTable.exists(instr.variable)) {
                    Variable var = symbolTable.get_variable(instr.variable);
                    Value val;
                    val.type = var.type;
                    val.int_value = var.int_value;
                    val.string_value = var.string_value;
                    stack.push_back(val);
                } else {
                    std::cerr << "Error: Variable '" << instr.variable << "' no declarada." << std::endl;
                }
                break;
            }
            case OpCode::STORE: {
                if (stack.empty()) {
                    std::cerr << "Error: La pila está vacía, no se puede STORE." << std::endl;
                } else {
                    Value val = stack.back(); stack.pop_back();
                    symbolTable.set_value(instr.variable, val);
                }
                break;
            }
            case OpCode::ADD: {
                if (stack.size() < 2) {
                    std::cerr << "Error: No hay suficientes valores en la pila para ADD." << std::endl;
                    break;
                }
                Value b = stack.back(); stack.pop_back();
                Value a = stack.back(); stack.pop_back();
                if (a.type == DataType::INTEGER && b.type == DataType::INTEGER) {
                    Value result;
                    result.type = DataType::INTEGER;
                    result.int_value = a.int_value + b.int_value;
                    stack.push_back(result);
                } else {
                    std::cerr << "Error: Tipos incompatibles para ADD." << std::endl;
                }
                break;
            }
            // Implementar SUB, MUL, DIV, EQ_OP, NEQ_OP, LT_OP, LTE_OP, GT_OP, GTE_OP, JMP, JMP_IF_FALSE
            case OpCode::SUB: {
                if (stack.size() < 2) {
                    std::cerr << "Error: No hay suficientes valores en la pila para SUB." << std::endl;
                    break;
                }
                Value b = stack.back(); stack.pop_back();
                Value a = stack.back(); stack.pop_back();
                if (a.type == DataType::INTEGER && b.type == DataType::INTEGER) {
                    Value result;
                    result.type = DataType::INTEGER;
                    result.int_value = a.int_value - b.int_value;
                    stack.push_back(result);
                } else {
                    std::cerr << "Error: Tipos incompatibles para SUB." << std::endl;
                }
                break;
            }
            case OpCode::MUL: {
                if (stack.size() < 2) {
                    std::cerr << "Error: No hay suficientes valores en la pila para MUL." << std::endl;
                    break;
                }
                Value b = stack.back(); stack.pop_back();
                Value a = stack.back(); stack.pop_back();
                if (a.type == DataType::INTEGER && b.type == DataType::INTEGER) {
                    Value result;
                    result.type = DataType::INTEGER;
                    result.int_value = a.int_value * b.int_value;
                    stack.push_back(result);
                } else {
                    std::cerr << "Error: Tipos incompatibles para MUL." << std::endl;
                }
                break;
            }
            case OpCode::DIV: {
                if (stack.size() < 2) {
                    std::cerr << "Error: No hay suficientes valores en la pila para DIV." << std::endl;
                    break;
                }
                Value b = stack.back(); stack.pop_back();
                if (b.int_value == 0) {
                    std::cerr << "Error: División por cero." << std::endl;
                    break;
                }
                Value a = stack.back(); stack.pop_back();
                if (a.type == DataType::INTEGER && b.type == DataType::INTEGER) {
                    Value result;
                    result.type = DataType::INTEGER;
                    result.int_value = a.int_value / b.int_value;
                    stack.push_back(result);
                } else {
                    std::cerr << "Error: Tipos incompatibles para DIV." << std::endl;
                }
                break;
            }
            case OpCode::EQ_OP: {
                if (stack.size() < 2) {
                    std::cerr << "Error: No hay suficientes valores en la pila para EQ_OP." << std::endl;
                    break;
                }
                Value b = stack.back(); stack.pop_back();
                Value a = stack.back(); stack.pop_back();
                Value result;
                result.type = DataType::INTEGER;
                result.int_value = (a.int_value == b.int_value) ? 1 : 0;
                stack.push_back(result);
                break;
            }
            // Implementar NEQ_OP, LT_OP, LTE_OP, GT_OP, GTE_OP de manera similar
            case OpCode::PRINT: {
                if (stack.empty()) {
                    std::cerr << "Error: La pila está vacía, no se puede PRINT." << std::endl;
                } else {
                    Value val = stack.back(); stack.pop_back();
                    if (val.type == DataType::INTEGER) {
                        std::cout << val.int_value << std::endl;
                    } else if (val.type == DataType::STRING) {
                        std::cout << val.string_value << std::endl;
                    }
                }
                break;
            }
            case OpCode::JMP: {
                pc = instr.operand_int;
                continue;
            }
            case OpCode::JMP_IF_FALSE: {
                if (stack.empty()) {
                    std::cerr << "Error: La pila está vacía en JMP_IF_FALSE." << std::endl;
                    return;
                }
                Value val = stack.back(); stack.pop_back();
                if (val.type != DataType::INTEGER) {
                    std::cerr << "Error: Valor no entero en JMP_IF_FALSE." << std::endl;
                    return;
                }
                if (val.int_value == 0) {
                    pc = instr.operand_int;
                    continue;
                }
                break;
            }
            default: {
                std::cerr << "OpCode desconocido." << std::endl;
                break;
            }
        }
        pc++;
    }
}

class Parser {
public:
    Parser(Lexer &lexer, SymbolTable &symbolTable)
        : lexer(lexer), symbolTable(symbolTable), current_token(lexer.next_token()) {}

    void parse() {
        while (current_token.type != Token::END_OF_FILE) {
            parse_statement();
        }

        // Imprimir y ejecutar todas las instrucciones acumuladas
        for (const auto& instr : instructions) {
            std::cout << "Generated instruction: " << instr.to_string() << std::endl;
        }
        execute_instructions(instructions, symbolTable);
    }

private:
    Lexer &lexer;
    SymbolTable &symbolTable;
    TokenValue current_token;
    std::vector<Instruction> instructions;

    void parse_statement() {
        if (current_token.type == Token::LOCAL || current_token.type == Token::IDENTIFIER) {
            parse_variable_declaration_or_assignment();
        } else if (current_token.type == Token::IF) {
            parse_if_statement();
        } else if (current_token.type == Token::WHILE) {
            parse_while_statement();
        } else if (current_token.type == Token::FOR) {
            parse_for_statement();
        } else if (current_token.type == Token::PRINT) {
            parse_print_statement();
        } else {
            std::cerr << "Error: Sentencia no reconocida." << std::endl;
            current_token = lexer.next_token(); // Avanzar para evitar ciclo infinito
        }
    }

    void parse_print_statement() {
        current_token = lexer.next_token(); // Consumir 'print'

        // Parsear la expresión a imprimir
        parse_expression();

        // Agregar la instrucción PRINT
        instructions.push_back(Instruction(OpCode::PRINT));

        // Opcionalmente, manejar fin de línea o punto y coma si tu lenguaje lo requiere
    }

    void parse_variable_declaration_or_assignment() {
        bool is_local = false;
        if (current_token.type == Token::LOCAL) {
            is_local = true;
            current_token = lexer.next_token();
        }

        if (current_token.type == Token::IDENTIFIER) {
            std::string var_name = current_token.value;
            current_token = lexer.next_token();

            if (current_token.type == Token::EQUALS) {
                current_token = lexer.next_token();
                // Agregar la variable a la tabla de símbolos antes de analizar la expresión
                if (!symbolTable.exists(var_name)) {
                    symbolTable.add_variable(var_name, DataType::INTEGER, is_local);
                }

                parse_expression();
                instructions.push_back(Instruction(OpCode::STORE, var_name, is_local));
                // No ejecutar las instrucciones aquí
            } else {
                // Declaración sin inicialización
                symbolTable.add_variable(var_name, DataType::INTEGER, is_local);
            }
        } else {
            std::cerr << "Error: Se esperaba un identificador." << std::endl;
            current_token = lexer.next_token(); // Avanzar para evitar ciclo infinito
        }
    }

    void parse_if_statement() {
        current_token = lexer.next_token(); // Consumir 'if'

        // Parsear la condición
        parse_expression();

        // Instrucción para salto condicional
        instructions.push_back(Instruction(OpCode::JMP_IF_FALSE, 0)); // La dirección de salto se actualizará más tarde
        size_t jmp_if_false_index = instructions.size() - 1;

        if (current_token.type == Token::THEN) {
            current_token = lexer.next_token(); // Consumir 'then'

            // Parsear el bloque 'then'
            while (current_token.type != Token::ELSE && current_token.type != Token::END) {
                parse_statement();
            }

            // Instrucción para saltar al final del 'if'
            instructions.push_back(Instruction(OpCode::JMP, 0)); // La dirección de salto se actualizará más tarde
            size_t jmp_end_index = instructions.size() - 1;

            size_t else_start_index = instructions.size();

            if (current_token.type == Token::ELSE) {
                current_token = lexer.next_token(); // Consumir 'else'

                // Parsear el bloque 'else'
                while (current_token.type != Token::END) {
                    parse_statement();
                }
            }

            if (current_token.type == Token::END) {
                current_token = lexer.next_token(); // Consumir 'end'
            } else {
                std::cerr << "Error: Se esperaba 'end' al final del bloque 'if'." << std::endl;
            }

            size_t end_index = instructions.size();

            // Actualizar las direcciones de salto
            instructions[jmp_if_false_index].operand_int = else_start_index;
            instructions[jmp_end_index].operand_int = end_index;

            // No ejecutar las instrucciones aquí

        } else {
            std::cerr << "Error: Se esperaba 'then' después de la condición 'if'." << std::endl;
        }
    }

    void parse_while_statement() {
        current_token = lexer.next_token(); // Consumir 'while'

        size_t loop_start_index = instructions.size(); // Marca el inicio del bucle

        // Parsear la condición
        parse_expression();

        // Instrucción para salto condicional
        instructions.push_back(Instruction(OpCode::JMP_IF_FALSE, 0)); // Dirección de salto a actualizar
        size_t jmp_if_false_index = instructions.size() - 1;

        if (current_token.type == Token::DO) {
            current_token = lexer.next_token(); // Consumir 'do'

            // Parsear el cuerpo del bucle
            while (current_token.type != Token::END) {
                parse_statement();
            }

            if (current_token.type == Token::END) {
                current_token = lexer.next_token(); // Consumir 'end'
            } else {
                std::cerr << "Error: Se esperaba 'end' al final del bucle 'while'." << std::endl;
            }

            // Instrucción para volver al inicio del bucle
            instructions.push_back(Instruction(OpCode::JMP, loop_start_index));

            // Actualizar la dirección de salto del JMP_IF_FALSE
            size_t loop_end_index = instructions.size();
            instructions[jmp_if_false_index].operand_int = loop_end_index;

        } else {
            std::cerr << "Error: Se esperaba 'do' después de la condición 'while'." << std::endl;
        }
    }

    void parse_for_statement() {
        current_token = lexer.next_token(); // Consumir 'for'

        if (current_token.type == Token::IDENTIFIER) {
            std::string var_name = current_token.value;
            current_token = lexer.next_token(); // Consumir el nombre de la variable

            if (current_token.type == Token::EQUALS) {
                current_token = lexer.next_token(); // Consumir '='

                // Parsear el valor inicial
                parse_expression();

                // Agregar la variable a la tabla de símbolos si no existe
                if (!symbolTable.exists(var_name)) {
                    symbolTable.add_variable(var_name, DataType::INTEGER, false);
                }

                // Instrucción para asignar el valor inicial a la variable
                instructions.push_back(Instruction(OpCode::STORE, var_name, false));

                if (current_token.type == Token::COMMA) {
                    current_token = lexer.next_token(); // Consumir ','

                    // Parsear el valor final
                    parse_expression();

                    // Nombre de la variable de fin
                    std::string end_var = var_name + "_end";

                    // Agregar la variable de fin a la tabla de símbolos si no existe
                    if (!symbolTable.exists(end_var)) {
                        symbolTable.add_variable(end_var, DataType::INTEGER, false);
                    }

                    // Almacenar el valor final en la variable de fin
                    instructions.push_back(Instruction(OpCode::STORE, end_var, false));

                    if (current_token.type == Token::DO) {
                        current_token = lexer.next_token(); // Consumir 'do'

                        size_t loop_start_index = instructions.size(); // Marca el inicio del bucle

                        // Verificar la condición de continuación del bucle
                        instructions.push_back(Instruction(OpCode::PUSH_VAR, var_name, false));
                        instructions.push_back(Instruction(OpCode::PUSH_VAR, end_var, false));
                        instructions.push_back(Instruction(OpCode::LTE_OP)); // Mientras var <= end_var

                        // Instrucción para salto condicional
                        instructions.push_back(Instruction(OpCode::JMP_IF_FALSE, 0)); // Dirección a actualizar
                        size_t jmp_if_false_index = instructions.size() - 1;

                        // Parsear el cuerpo del bucle
                        while (current_token.type != Token::END) {
                            parse_statement();
                        }

                        if (current_token.type == Token::END) {
                            current_token = lexer.next_token(); // Consumir 'end'
                        } else {
                            std::cerr << "Error: Se esperaba 'end' al final del bucle 'for'." << std::endl;
                        }

                        // Incrementar la variable de control
                        instructions.push_back(Instruction(OpCode::PUSH_VAR, var_name, false));
                        instructions.push_back(Instruction(OpCode::PUSH_INT, 1));
                        instructions.push_back(Instruction(OpCode::ADD));
                        instructions.push_back(Instruction(OpCode::STORE, var_name, false));

                        // Volver al inicio del bucle
                        instructions.push_back(Instruction(OpCode::JMP, loop_start_index));

                        // Actualizar la dirección de salto del JMP_IF_FALSE
                        size_t loop_end_index = instructions.size();
                        instructions[jmp_if_false_index].operand_int = loop_end_index;

                    } else {
                        std::cerr << "Error: Se esperaba 'do' después del rango en el bucle 'for'." << std::endl;
                    }
                } else {
                    std::cerr << "Error: Se esperaba ',' en el bucle 'for'." << std::endl;
                }
            } else {
                std::cerr << "Error: Se esperaba '=' después del nombre de la variable en el bucle 'for'." << std::endl;
            }
        } else {
            std::cerr << "Error: Se esperaba un identificador después de 'for'." << std::endl;
        }
    }

    void parse_expression() {
        parse_relational_expression();
        // Si quieres manejar operadores lógicos como 'and', 'or', puedes extender aquí
    }

    void parse_relational_expression() {
        parse_arithmetic_expression();
        while (current_token.type == Token::EQ || current_token.type == Token::NEQ ||
               current_token.type == Token::LT || current_token.type == Token::LTE ||
               current_token.type == Token::GT || current_token.type == Token::GTE) {
            Token op = current_token.type;
            current_token = lexer.next_token();
            parse_arithmetic_expression();
            switch (op) {
                case Token::EQ:
                    instructions.push_back(Instruction(OpCode::EQ_OP));
                    break;
                case Token::NEQ:
                    instructions.push_back(Instruction(OpCode::NEQ_OP));
                    break;
                case Token::LT:
                    instructions.push_back(Instruction(OpCode::LT_OP));
                    break;
                case Token::LTE:
                    instructions.push_back(Instruction(OpCode::LTE_OP));
                    break;
                case Token::GT:
                    instructions.push_back(Instruction(OpCode::GT_OP));
                    break;
                case Token::GTE:
                    instructions.push_back(Instruction(OpCode::GTE_OP));
                    break;
                default:
                    break;
            }
        }
    }

    void parse_arithmetic_expression() {
        parse_term();
        while (current_token.type == Token::PLUS || current_token.type == Token::MINUS) {
            Token op = current_token.type;
            current_token = lexer.next_token();
            parse_term(); // Asegúrate de llamar a parse_term() para el segundo operando
            if (op == Token::PLUS) {
                instructions.push_back(Instruction(OpCode::ADD));
            } else if (op == Token::MINUS) {
                instructions.push_back(Instruction(OpCode::SUB));
            }
        }
    }

    void parse_term() {
        parse_factor();
        while (current_token.type == Token::MULTIPLY || current_token.type == Token::DIVIDE) {
            Token op = current_token.type;
            current_token = lexer.next_token();
            parse_factor();
            if (op == Token::MULTIPLY) {
                instructions.push_back(Instruction(OpCode::MUL));
            } else if (op == Token::DIVIDE) {
                instructions.push_back(Instruction(OpCode::DIV));
            }
        }
    }

    void parse_factor() {
        if (current_token.type == Token::NUMBER) {
            int value = std::stoi(current_token.value);
            instructions.push_back(Instruction(OpCode::PUSH_INT, value));
            current_token = lexer.next_token();
        } else if (current_token.type == Token::STRING_LITERAL) {
            std::string value = current_token.value;
            instructions.push_back(Instruction(OpCode::PUSH_STRING, value, 0)); // Añade '0' como dummy
            current_token = lexer.next_token();
        } else if (current_token.type == Token::IDENTIFIER) {
            std::string var_name = current_token.value;
            if (symbolTable.exists(var_name)) {
                instructions.push_back(Instruction(OpCode::PUSH_VAR, var_name, false));
            } else {
                std::cerr << "Error: Variable '" << var_name << "' no declarada." << std::endl;
            }
            current_token = lexer.next_token();
        } else if (current_token.type == Token::LPAREN) {
            current_token = lexer.next_token(); // Consumir '('
            parse_expression();
            if (current_token.type == Token::RPAREN) {
                current_token = lexer.next_token(); // Consumir ')'
            } else {
                std::cerr << "Error: Se esperaba ')'." << std::endl;
            }
        } else {
            std::cerr << "Error: Se esperaba un número, cadena, variable o '('" << std::endl;
            current_token = lexer.next_token(); // Avanzar para evitar ciclo infinito
        }
    }
}; // Asegúrate de cerrar la clase con '};'




int main() {
    // Variable para almacenar el contenido del archivo
    std::string input;

    // Abrir el archivo 'test.lua' en modo lectura
    std::ifstream file("test.lua");

    // Verificar si el archivo se abrió correctamente
    if (!file.is_open()) {
        std::cerr << "Error: No se pudo abrir el archivo 'test.lua'." << std::endl;
        return 1;
    }

    // Leer el contenido completo del archivo en la variable 'input'
    std::string line;
    while (std::getline(file, line)) {
        input += line + "\n";
    }

    // Cerrar el archivo
    file.close();

    std::cout << "Main: Starting the compilation process." << std::endl;

    Lexer lexer(input);
    SymbolTable symbolTable;
    Parser parser(lexer, symbolTable);

    parser.parse();
    symbolTable.print_table();

    std::cout << "Main: Compilation process finished." << std::endl;
    return 0;
}

