#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_SYMBOLS 512
#define MAX_TOKENS 32
#define MAX_VALID_LINES 128
#define MAX_TEMP_REGS 32

//JEG C. TANQUIZON CSC112_BN01
//JOHNWENCES FABE DUMANGCAS CSC112_BN01

// Token Types
typedef enum {
  TOKEN_KEYWORD,
  TOKEN_VARIABLE,
  TOKEN_INTEGER,
  TOKEN_CHAR,
  TOKEN_OPERATOR,
  TOKEN_DELIMITER,
  TOKEN_ERROR,
  TOKEN_NONE
} TokenType;

typedef enum { 
  TYPE_INT, 
  TYPE_CHAR, 
  TYPE_UNKNOWN 
} VarType;

typedef struct {
  TokenType type;
  char value[32];
} Token;

// Valid Line Structure
typedef struct {
  char line[512];
  Token tokens[MAX_TOKENS];
  int count;
  int line_no;
} ValidLine;

// Symbol Table (now with addresses)
typedef struct {
  char var[32];
  int addr;
  VarType type; // store int or char type
  int declared_line;
} Symbol;

static Symbol symbol_table[MAX_SYMBOLS];
static int symbol_count = 0;

// Temp Registers
static char temp_regs[MAX_TEMP_REGS][16];
static int temp_count = 0;

// Keywords
static const char *keywords[] = {"int", "char", NULL};

static int is_keyword(const char *word) {
  for (int i = 0; keywords[i] != NULL; i++) {
    if (strcmp(keywords[i], word) == 0)
      return 1;
  }
  return 0;
}

// Symbol Management

static int find_symbol(const char *name) {
  for (int i = 0; i < symbol_count; i++) {
    if (strcmp(symbol_table[i].var, name) == 0)
      return i;
  }
  return -1;//wala ni exist
}

static int add_symbol(const char *name, VarType type, int line_no) {

  int idx = find_symbol(name);

  if (idx >= 0) {
    printf("     Semantic error: redeclaration of variable '%s' at line %d "
           "(first declared at line %d)\n",
           name, line_no, symbol_table[idx].declared_line);
    return 0;
  }

  if (symbol_count >= MAX_SYMBOLS)
    return -1;

  strcpy(symbol_table[symbol_count].var, name);
  symbol_table[symbol_count].addr = 0;
  symbol_table[symbol_count].type = type;

  // line checker
  symbol_table[symbol_count].declared_line = line_no;

  symbol_count++;
  return 1; // SUCCESS
}

static VarType get_variable_type(const char *name) {
  int idx = find_symbol(name);
  if (idx < 0)
    return TYPE_UNKNOWN;
  return symbol_table[idx].type;
}

static int collect_symbols(Token *tokens, int count, int line_no) {
  if (tokens[0].type == TOKEN_KEYWORD) {
    // === DECLARATION: int x, y = 5, z = 10; ===
    VarType decl_type =
        strcmp(tokens[0].value, "int") == 0 ? TYPE_INT : TYPE_CHAR;

    int pos = 1; // Start after type keyword
    int error_occurred = 0;

    while (pos < count - 1) { // Stop before semicolon
      // Expect a variable
      if (tokens[pos].type != TOKEN_VARIABLE) {
        printf("     Semantic error: expected variable name in declaration "
               "(line %d)\n",
               line_no);
        return 0;
      }

      const char *var_name = tokens[pos].value;
      if (!add_symbol(var_name, decl_type, line_no)) {
        return 0; // Redeclaration error already printed
      }
      pos++; // Move past variable

      // Optional: = expression
      if (pos < count - 1 && tokens[pos].type == TOKEN_OPERATOR &&
          strcmp(tokens[pos].value, "=") == 0) {
        pos++; // Move past =
        if (pos >= count - 1) {
          printf("     Semantic error: missing expression after '=' in "
                 "declaration (line %d)\n",
                 line_no);
          return 0;
        }

        // Isolate this expression: scan until , or ;
        int expr_start = pos;
        while (pos < count - 1 && !(tokens[pos].type == TOKEN_DELIMITER &&
                                    (strcmp(tokens[pos].value, ",") == 0 ||
                                     strcmp(tokens[pos].value, ";") == 0))) {
          pos++;
        }
        int expr_end = pos; // Now at , or ;

        // Check variables in this isolated expression are declared
        for (int j = expr_start; j < expr_end; j++) {
          if (tokens[j].type == TOKEN_VARIABLE &&
              find_symbol(tokens[j].value) < 0) {
            printf("     Semantic error: undeclared variable '%s' used in "
                   "initializer (line %d)\n",
                   tokens[j].value, line_no);
            error_occurred = 1;
          }
        }
        if (error_occurred)
          return 0;
      }

      // After var or var=expr, expect , or ;
      if (pos < count - 1) {
        if (tokens[pos].type == TOKEN_DELIMITER) {
          if (strcmp(tokens[pos].value, ",") == 0) {
            pos++; // Next variable
          } else if (strcmp(tokens[pos].value, ";") == 0) {
            break; // End of declaration
          } else {
            printf("     Semantic error: unexpected delimiter '%s' in "
                   "declaration (line %d)\n",
                   tokens[pos].value, line_no);
            return 0;
          }
        } else {
          printf("     Semantic error: expected ',' or ';' after declaration "
                 "item (line %d)\n",
                 line_no);
          return 0;
        }
      } else {
        break;
      }
    }
  } else if (tokens[0].type == TOKEN_VARIABLE &&
             tokens[1].type == TOKEN_OPERATOR &&
             strcmp(tokens[1].value, "=") == 0) {
    // === ASSIGNMENT: x = f + t; === (unchanged)
    const char *lhs = tokens[0].value;

    // LHS must be declared
    if (find_symbol(lhs) < 0) {
      printf("     Semantic error: assignment to undeclared variable '%s' "
             "(line %d)\n",
             lhs, line_no);
      return 0;
    }

    // RHS: check all variables are declared
    for (int i = 2; i < count - 1; i++) {
      if (tokens[i].type == TOKEN_VARIABLE) {
        if (find_symbol(tokens[i].value) < 0) {
          printf("     Semantic error: undeclared variable '%s' in expression "
                 "(line %d)\n",
                 tokens[i].value, line_no);
          return 0;
        }
      }
    }
  }

  return 1; // All goods
}

static const char *allocate_temp() {
  if (temp_count >= MAX_TEMP_REGS)
    return NULL;
  snprintf(temp_regs[temp_count], 16, "R%d", temp_count + 1);
  return temp_regs[temp_count++];
}

// Semantic Error Detection
// Check for literal division by zero (e.g., 5 / 0)
static int check_literal_division_by_zero(Token *tokens, int start, int end) {
  for (int i = start; i < end - 1; i++) {
    if (tokens[i].type == TOKEN_OPERATOR && strcmp(tokens[i].value, "/") == 0) {
      // Check if next token is literal 0
      if (tokens[i + 1].type == TOKEN_INTEGER &&
          strcmp(tokens[i + 1].value, "0") == 0) {
        return 1; // Division by zero detected
      }
    }
  }
  return 0;
}

// Check for invalid character literal (e.g., 'ab' or '')
static int check_invalid_char_literal(Token *tokens, int count) {
  for (int i = 0; i < count; i++) {
    if (tokens[i].type == TOKEN_CHAR) {
      // Valid char: 'a' (length 3) or '\n' (length 4 for escape sequences)
      int len = strlen(tokens[i].value);
      if (len < 3 || len > 4) {
        return 1; // Invalid
      }
      // Check format: must start and end with '
      if (tokens[i].value[0] != '\'' || tokens[i].value[len - 1] != '\'') {
        return 1; // Invalid
      }
      // Check content: empty or too many chars between quotes
      if (len == 3 && tokens[i].value[1] == '\'') {
        return 1; // Empty: ''
      }
      if (len == 4 && tokens[i].value[2] != '\'') {
        // More than 1 char: 'ab'
        if (tokens[i].value[1] != '\\') { // Unless it's escape sequence
          return 1;
        }
      }
    }
  }
  return 0;
}

// Check for integer overflow (too large integers)
static int check_integer_overflow(Token *tokens, int count) {
  for (int i = 0; i < count; i++) {
    if (tokens[i].type == TOKEN_INTEGER) {
      // Check if number is too large (> 64-bit max)
      if (strlen(tokens[i].value) > 20) {
        return 1; // Likely overflow
      }
      // Check against actual limits (simplified)
      if (strlen(tokens[i].value) == 20) {
        if (strcmp(tokens[i].value, "18446744073709551615") > 0) {
          return 1; // Overflow
        }
      }
    }
  }
  return 0;
}

// Check for type mismatch in char assignment to int (or vice versa)
static int check_type_mismatch(Token *tokens, int count) {
  // Check if char is being assigned to int declaration or vice versa
  if (count >= 5 && tokens[0].type == TOKEN_KEYWORD) {
    const char *type = tokens[0].value;

    // Find the assignment value (after '=')
    for (int i = 2; i < count; i++) {
      if (tokens[i].type == TOKEN_OPERATOR &&
          strcmp(tokens[i].value, "=") == 0) {
        // Check what's being assigned
        if (i + 1 < count) {
          // int var = 'c'; is invalid (char to int)
          if (strcmp(type, "int") == 0 && tokens[i + 1].type == TOKEN_CHAR) {
            return 1; // Type mismatch
          }
          // char var = 123; is invalid (int to char)
          if (strcmp(type, "char") == 0 &&
              tokens[i + 1].type == TOKEN_INTEGER) {
            return 1; // Type mismatch
          }
        }
        break;
      }
    }
  }
  return 0;
}

// Check for operator without operands (e.g., "5 + * 3")
static int check_missing_operand(Token *tokens, int count) {
  for (int i = 1; i < count - 1; i++) {
    // Two operators in a row (except unary cases)
    if (tokens[i].type == TOKEN_OPERATOR &&
        tokens[i + 1].type == TOKEN_OPERATOR) {
      // Allow some exceptions, but generally invalid
      return 1;
    }
  }
  return 0;
}

/* 
   =========================================================
    MACHINE CODE WRITER – adds binary and hex translation
   ========================================================= 
*/
void write_machine_code(FILE *out, const char *instr, int rd, int rs, int rt,
                        int imm, const char *comment) {
  unsigned int opcode = 0, funct = 0, machine = 0;

  if (strcmp(instr, "DADDIU") == 0) { // I-type
    opcode = 0b011001; //0b : binary
    machine = (opcode << 26) | (rs << 21) | (rt << 16) | (imm & 0xFFFF);
  } else if (strcmp(instr, "DADDU") == 0) { // R-type
    opcode = 0b000000;
    funct = 0b101101;
    machine = (opcode << 26) | (rs << 21) | (rt << 16) | (rd << 11) | funct;
  } else if (strcmp(instr, "DSUBU") == 0) { // R-type
    opcode = 0b000000;
    funct = 0b101111;
    machine = (opcode << 26) | (rs << 21) | (rt << 16) | (rd << 11) | funct;
  } else if (strcmp(instr, "DMULTU") == 0) { // R-type (no rd)
    opcode = 0b000000;
    funct = 0b011001;
    machine = (opcode << 26) | (rs << 21) | (rt << 16) | funct;
  } else if (strcmp(instr, "DDIVU") == 0) { // R-type (no rd)
    opcode = 0b000000;
    funct = 0b011011;
    machine = (opcode << 26) | (rs << 21) | (rt << 16) | funct;
  } else if (strcmp(instr, "LD") == 0) { // I-type load
    opcode = 0b110111;
    machine = (opcode << 26) | (rs << 21) | (rt << 16) | (imm & 0xFFFF);
  } else if (strcmp(instr, "SD") == 0) { // I-type store
    opcode = 0b111111;
    machine = (opcode << 26) | (rs << 21) | (rt << 16) | (imm & 0xFFFF);
  } else if (strcmp(instr, "MFLO") == 0) { // R-type
    opcode = 0b000000;
    funct = 0b010000;
    machine = (opcode << 26) | (0 << 21) | (0 << 16) | (rd << 11) | funct;
  }

  // Convert to 32-bit binary string
  char binary[33];
  for (int i = 31; i >= 0; i--)
    binary[31 - i] = (machine & (1u << i)) ? '1' : '0';
  binary[32] = '\0';

  // Write to output file with comment
  fprintf(out, " >> %s >> 0x%08X %s\n", binary, machine, comment);
}

// Tokenizer

static int tokenize(const char *line, Token tokens[], int max_tokens) {
  int count = 0;

  while (*line && count < max_tokens) {
    // Skip whitespace
    if (isspace((unsigned char)*line)) {
      line++;
      continue;
    }

    Token *tok = &tokens[count];
    tok->type = TOKEN_NONE;

    // Single-character operators and delimiters
    if (strchr("=+-*/(),;", *line)) {
      tok->value[0] = *line;
      tok->value[1] = '\0';
      tok->type = (strchr("=+-*/", *line)) ? TOKEN_OPERATOR : TOKEN_DELIMITER;
      count++;
      line++;
      continue;
    }

    // Character literals: 'a', 'ab', 'abc' etc - read until closing quote 


    if (*line == '\'') {
      int i = 0;
      tok->value[i++] = *line++; // opening quote '
      // Read all content until closing quote (no space limit)
      while (*line && *line != '\'' && i < 30) {
        tok->value[i++] = *line++;
      }
      if (*line == '\'') {
        tok->value[i++] = *line++; // closing quote '
      }
      tok->value[i] = '\0';
      tok->type = TOKEN_CHAR;
      count++;
      continue;
    }

    // Identifiers and keywords
    if (isalpha((unsigned char)*line) || *line == '_') {
      int i = 0;
      while ((isalnum((unsigned char)*line) || *line == '_') && i < 31)
        tok->value[i++] = *line++;
      tok->value[i] = '\0';
      tok->type = is_keyword(tok->value) ? TOKEN_KEYWORD : TOKEN_VARIABLE;
      count++;
      continue;
    }

    // Integer literals
    if (isdigit((unsigned char)*line)) {
      int i = 0;
      while (isdigit((unsigned char)*line) && i < 31)
        tok->value[i++] = *line++;
      tok->value[i] = '\0';
      tok->type = TOKEN_INTEGER;
      count++;
      continue;
    }

    // Unknown token
    tok->value[0] = *line;
    tok->value[1] = '\0';
    tok->type = TOKEN_ERROR;
    count++;
    line++;
  }

  return count;
}

// Syntax Validation
// Function to validate multi-variable declarations (handles single as special
// case)
// okay nani
static int is_valid_multi_declaration(Token *tokens, int count, int line_no,
                                      FILE *out) {
  if (count < 3) {
    printf("     Incomplete declaration\n");
    return 0;
  }

  if (tokens[0].type != TOKEN_KEYWORD)
    return 0;

  if (tokens[count - 1].type != TOKEN_DELIMITER ||
      strcmp(tokens[count - 1].value, ";") != 0)
    return 0;

  // Process each variable-expression pair
  int pos = 1; // start after type keyword
  char vars[256] = {0};
  int var_count = 0;

  while (pos < count - 1) { // stop before semicolon
    // Expect a variable
    if (tokens[pos].type != TOKEN_VARIABLE) {
      printf("     Expected variable name\n");
      return 0;
    }
    if (var_count > 0)
      strcat(vars, ", ");
    strcat(vars, tokens[pos].value);
    pos++;
    var_count++;

    // Optional initialization
    if (pos < count - 1 && tokens[pos].type == TOKEN_OPERATOR &&
        strcmp(tokens[pos].value, "=") == 0) {
      pos++; // move to start of expression
      if (pos >= count - 1) {
        printf("     Missing expression after '='\n");
        return 0;
      }

      // Expression validation (same as original)
      int i = pos;
      int expect_operand = 1;
      while (i < count - 1 && !(tokens[i].type == TOKEN_DELIMITER &&
                                strcmp(tokens[i].value, ",") == 0)) {
        // Skip parentheses
        if (tokens[i].type == TOKEN_DELIMITER &&
            (strcmp(tokens[i].value, "(") == 0 ||
             strcmp(tokens[i].value, ")") == 0)) {
          i++;
          continue;
        }

        if (expect_operand) {
          if (tokens[i].type == TOKEN_INTEGER ||
              tokens[i].type == TOKEN_VARIABLE ||
              tokens[i].type == TOKEN_CHAR ||
              (tokens[i].type == TOKEN_DELIMITER &&
               strcmp(tokens[i].value, "(") == 0)) {
            expect_operand = 0;
          } else {
            printf("     Invalid expression syntax\n");
            return 0;
          }
        } else {
          if (tokens[i].type == TOKEN_OPERATOR &&
              strcmp(tokens[i].value, "=") != 0) {
            expect_operand = 1;
          } else {
            printf("     Invalid expression syntax\n");
            return 0;
          }
        }
        i++;
      }

      if (expect_operand) {
        printf("     Expression ends with operator\n");
        return 0;
      }

      pos = i; // move pos to comma or next variable
    }

    // After variable or variable=expr, check what comes next
    if (pos < count - 1) {
      if (tokens[pos].type == TOKEN_DELIMITER) {
        if (strcmp(tokens[pos].value, ",") == 0) {
          // Comma found move to next variable
          pos++;
          // Make sure there is a variable after comma
          if (pos >= count - 1 || tokens[pos].type != TOKEN_VARIABLE) {
            printf("     Trailing comma or missing variable\n");
            return 0;
          }
        } else if (strcmp(tokens[pos].value, ";") == 0) {
          // Semicolon end of declaration
          break;
        } else {
          printf("     Unexpected delimiter '%s'\n", tokens[pos].value);
          return 0;
        }
      } else {
        printf("     Expected ',' or ';'\n");
        return 0;
      }
    }
  }

  //Consume all trailing semicolons
  while (pos < count && tokens[pos].type == TOKEN_DELIMITER &&
        strcmp(tokens[pos].value, ";") == 0) {
      pos++;
  }

  // If anything left after that  syntax error
  if (pos < count) {
      printf("     Garbage after declaration\n");
      return 0;
  }

  return 1;
}
// Helper: check if variable is declared
static int is_declared_var(const char *name) { 

  return find_symbol(name) >= 0; 

}

static int is_valid_assignment(Token *tokens, int count, int line_no, FILE *out) {
  
  if (count != 4)
    return 0;

  if (tokens[0].type != TOKEN_VARIABLE) {
    return 0;
  }

  if (tokens[1].type != TOKEN_OPERATOR || strcmp(tokens[1].value, "=") != 0) {
    return 0;
  }

  if (tokens[2].type != TOKEN_INTEGER && tokens[2].type != TOKEN_VARIABLE &&
      tokens[2].type != TOKEN_CHAR) {
    return 0;
  }

  if (tokens[3].type != TOKEN_DELIMITER || strcmp(tokens[3].value, ";") != 0) {
    return 0;
  }

  return 1;
}

static int is_valid_arithmetic(Token *tokens, int count, int line_no,
                               FILE *out) {
  if (count < 4)
    return 0;

  if (tokens[0].type != TOKEN_VARIABLE)
    return 0;

  if (tokens[1].type != TOKEN_OPERATOR || strcmp(tokens[1].value, "=") != 0)
    return 0;

  if (tokens[count - 1].type != TOKEN_DELIMITER ||
      strcmp(tokens[count - 1].value, ";") != 0)
    return 0;

  int i = 2;
  // allow integer, variable, or '(' after '='
  if (tokens[i].type != TOKEN_INTEGER && tokens[i].type != TOKEN_VARIABLE &&
      !(tokens[i].type == TOKEN_DELIMITER && strcmp(tokens[i].value, "(") == 0))
    return 0;
  i++;

  while (i < count - 1) {
    // allow parentheses anywhere
    if (tokens[i].type == TOKEN_DELIMITER &&
        (strcmp(tokens[i].value, "(") == 0 ||
         strcmp(tokens[i].value, ")") == 0)) {
      i++;
      continue;
    }

    // operators + - * /
    if (tokens[i].type == TOKEN_OPERATOR) {
      if (strcmp(tokens[i].value, "=") == 0)
        return 0;
      i++;
      continue;
    }

    // operands: integer, variable, or '('
    if (tokens[i].type == TOKEN_INTEGER || tokens[i].type == TOKEN_VARIABLE ||
        (tokens[i].type == TOKEN_DELIMITER &&
         strcmp(tokens[i].value, "(") == 0)) {
      i++;
      continue;
    }

    return 0;
  }

  return 1;
}

// Semantic Validation
static int validate_semantics(Token *tokens, int count, int line_no) {

  if (tokens[0].type == TOKEN_VARIABLE && tokens[1].type == TOKEN_OPERATOR &&
      strcmp(tokens[1].value, "=") == 0) {

    VarType lhs_type = get_variable_type(tokens[0].value);
    if (lhs_type == TYPE_UNKNOWN) {
      printf("     Variable '%s' used before declaration\n", tokens[0].value);
      return 0;
    }

    // Handle simple assignment: x = 5; or x = y; or x = 'a';
    if (count == 4) {
      Token *rhs = &tokens[2];

      if (rhs->type == TOKEN_INTEGER) {
        int val = atoi(rhs->value);
        if (lhs_type == TYPE_CHAR) {
          if (val < -128 || val > 127) { // Strict: signed char range
            printf("     Semantic error: integer literal %d out of signed char "
                   "range (-128 to 127)\n",
                   val);
            return 0;
          }
          if (val < 0 || val > 255) {
            printf("     Semantic error: integer literal %d out of char range "
                   "(0-255 for unsigned)\n",
                   val);
            return 0;
          }
          // Even if it fits, warn or error on narrowing
          printf("     Semantic error: implicit narrowing from int literal to "
                 "char (use explicit cast or char literal)\n");
          return 0; // STRICT: reject all int → char assignments
        }
      } else if (rhs->type == TOKEN_CHAR) {
        if (lhs_type == TYPE_INT) {
          // char literal to int is OK (promotion)
        } else if (lhs_type == TYPE_CHAR) {
          // char = 'a'; OK
        }
      } else if (rhs->type == TOKEN_VARIABLE) {
        VarType rhs_type = get_variable_type(rhs->value);
        if (rhs_type == TYPE_UNKNOWN) {
          printf("     Undeclared variable on RHS: '%s'\n", rhs->value);
          return 0;
        }
        if (lhs_type == TYPE_CHAR && rhs_type == TYPE_INT) {
          printf("     Semantic error: cannot assign int variable '%s' to char "
                 "variable '%s' without cast\n",
                 rhs->value, tokens[0].value);
          return 0;
        }
        // int = char is OK
        // char = char is OK
      }
    }
    // For complex expressions: we don't do full type inference yet
    // But at least catch obvious cases above
  }

  // Check for literal division by zero
  if (check_literal_division_by_zero(tokens, 0, count)) {
    printf("     Division by zero\n");
    return 0;
  }

  // Check for invalid character literals
  if (check_invalid_char_literal(tokens, count)) {
    printf("     Invalid character literal\n");
    return 0;
  }
  // Check for undeclared variables (use before declaration)
  for (int i = 0; i < count; i++) {
    if (tokens[i].type == TOKEN_VARIABLE) {
      // skip variable declaration (first token if keyword)
      if (i > 0 && tokens[0].type == TOKEN_KEYWORD)
        continue;

      if (!is_declared_var(tokens[i].value)) {
        printf("     Undeclared variable '%s'\n", tokens[i].value);
        return 0;
      }
    }
  }

  // Check for integer overflow
  if (check_integer_overflow(tokens, count)) {
    printf("     Integer overflow\n");
    return 0;
  }

  // Check for type mismatch
  if (check_type_mismatch(tokens, count)) {
    printf("     Type mismatch (char vs int)\n");
    return 0;
  }

  // Check for missing operand
  if (check_missing_operand(tokens, count)) {
    printf("     Missing operand\n");
    return 0;
  }

  return 1;
}

static int process_one_statement_semantically(Token *tokens, int count, int line_no) {
    if (count == 0) return 1;

    // Skip if it's just a semicolon (empty statement)
    if (count == 1 && tokens[0].type == TOKEN_DELIMITER && strcmp(tokens[0].value, ";") == 0) {
        return 1;
    }

    // If it's a declaration → collect symbols
    if (tokens[0].type == TOKEN_KEYWORD) {
        return collect_symbols(tokens, count, line_no);
    }

    // Otherwise → normal semantic checks (use before decl, types, etc.)
    return validate_semantics(tokens, count, line_no);
}

// Helper: check parentheses balance
static int check_parentheses_balance(Token *tokens, int count) {
  int balance = 0;
  for (int i = 0; i < count; i++) {
    if (tokens[i].type == TOKEN_DELIMITER && strcmp(tokens[i].value, "(") == 0)
      balance++;
    else if (tokens[i].type == TOKEN_DELIMITER &&
             strcmp(tokens[i].value, ")") == 0)
      balance--;
    if (balance < 0)
      return 0;
  }
  return balance == 0;
}

// Helper: detect invalid operator sequences
static int has_invalid_operator_sequence(Token *tokens, int count) {
  for (int i = 1; i < count - 1; i++) {
    if (tokens[i].type == TOKEN_OPERATOR &&
        tokens[i + 1].type == TOKEN_OPERATOR) {
      return 1;
    }
  }
  return 0;
}

// Helper: detect empty parentheses
static int has_empty_parentheses(Token *tokens, int count) {
  for (int i = 0; i < count - 1; i++) {
    if (tokens[i].type == TOKEN_DELIMITER &&
        strcmp(tokens[i].value, "(") == 0 &&
        tokens[i + 1].type == TOKEN_DELIMITER &&
        strcmp(tokens[i + 1].value, ")") == 0) {
      return 1;
    }
  }
  return 0;
}

static int validate_syntax(Token *tokens, int count, int line_no, FILE *out) {
  if (count == 0) return 1;

  // Check for unknown tokens
  for (int i = 0; i < count; i++) {
    if (tokens[i].type == TOKEN_ERROR) {
      printf("     Unknown token '%s'\n", tokens[i].value);
      return 0;
    }
  }

  int pos = 0;

  while (pos < count) {
    // Skip leading/trailing semicolons between statements
    while (pos < count && tokens[pos].type == TOKEN_DELIMITER &&
           strcmp(tokens[pos].value, ";") == 0) {
      pos++;
    }
    if (pos >= count) break;

    // ==================== DECLARATION ====================
    if (tokens[pos].type == TOKEN_KEYWORD) {
      if (strcmp(tokens[pos].value, "int") != 0 && strcmp(tokens[pos].value, "char") != 0) {
        printf("     Invalid type keyword '%s'\n", tokens[pos].value);
        return 0;
      }

      int decl_start = pos;
      int decl_end = pos;

      // Find the terminating semicolon
      while (decl_end < count) {
        if (tokens[decl_end].type == TOKEN_DELIMITER &&
            strcmp(tokens[decl_end].value, ";") == 0) {
          decl_end++;  // include the ;
          break;
        }
        decl_end++;
      }
      if (decl_end > count) {
        printf("     Missing semicolon after declaration\n");
        return 0;
      }

      // Validate only this one declaration
      if (!is_valid_multi_declaration(tokens + decl_start, decl_end - decl_start, line_no, out)) {
        return 0;
      }

      pos = decl_end;
      continue;
    }

    // ==================== ASSIGNMENT / ARITHMETIC ====================
    if (tokens[pos].type == TOKEN_VARIABLE &&
        pos + 1 < count &&
        tokens[pos + 1].type == TOKEN_OPERATOR &&
        strcmp(tokens[pos + 1].value, "=") == 0) {

      int stmt_start = pos;
      int stmt_end = pos + 2;

      // Find the terminating semicolon
      while (stmt_end < count) {
        if (tokens[stmt_end].type == TOKEN_DELIMITER &&
            strcmp(tokens[stmt_end].value, ";") == 0) {
          stmt_end++;
          break;
        }
        stmt_end++;
      }
      if (stmt_end > count) stmt_end = count;

      int stmt_len = stmt_end - stmt_start;
      Token *stmt = tokens + stmt_start;

      int valid = 0;
      if (stmt_len == 4) {
        valid = is_valid_assignment(stmt, stmt_len, line_no, out);
      } else if (stmt_len >= 5) {
        valid = is_valid_arithmetic(stmt, stmt_len, line_no, out);
      }

      if (!valid) {
        printf("     Invalid assignment/arithmetic statement\n");
        return 0;
      }

      pos = stmt_end;
      continue;
    }

    // If nothing matches
    printf("     Unrecognized statement at token '%s'\n", tokens[pos].value);
    return 0;
  }

  // Global checks (parentheses, operator sequences, etc.)
  if (!check_parentheses_balance(tokens, count)) {
    printf("     Unbalanced parentheses\n");
    return 0;
  }
  if (has_invalid_operator_sequence(tokens, count)) {
    printf("     Invalid operator sequence\n");
    return 0;
  }
  if (has_empty_parentheses(tokens, count)) {
    printf("     Empty parentheses not allowed\n");
    return 0;
  }

  return 1; 
}

/* 
  =========================
     Expression Parser
  =========================
*/
static const char *load_operand(Token *operand, FILE *out);

static const char *parse_expression(Token *tokens, int count, int *idx,
                                    FILE *out);
static const char *parse_term(Token *tokens, int count, int *idx, FILE *out);
static const char *parse_factor(Token *tokens, int count, int *idx, FILE *out);

static const char *load_operand(Token *operand, FILE *out) { //the thing we want to load, output file to write asscode
  const char *temp_reg = allocate_temp();
  if (!temp_reg) //check if no more temp registers
    return NULL;
  int reg_num = atoi(temp_reg + 1); // Skip 'R', read number
  char comment[128];

  if (operand->type == TOKEN_VARIABLE) {
    int sym_idx = find_symbol(operand->value);
    if (sym_idx < 0) {
      // Undeclared variable
      return NULL;
    }
    int offset = symbol_table[sym_idx].addr;
    snprintf(comment, sizeof(comment), "[ load %s to %s ]", operand->value,
             temp_reg);
    fprintf(out, "LD %s, %s(R0)\n", temp_reg, operand->value);
    write_machine_code(out, "LD", 0, 0, reg_num, offset, comment);
  } else if (operand->type == TOKEN_INTEGER) {
    int val = atoi(operand->value);
    snprintf(comment, sizeof(comment), "[ load %d to %s ]", val, temp_reg);
    fprintf(out, "DADDIU %s, R0, #%d\n", temp_reg, val);
    write_machine_code(out, "DADDIU", 0, 0, reg_num, val, comment);
  } else if (operand->type == TOKEN_CHAR) {
    char ch = operand->value[1];
    int val = (int)(unsigned char)ch;//force it to be 0-255
    snprintf(comment, sizeof(comment), "[ load '%c' (%d) to %s ]", ch, val,
             temp_reg);
    fprintf(out, "DADDIU %s, R0, #%d\n", temp_reg, val);
    write_machine_code(out, "DADDIU", 0, 0, reg_num, val, comment);
  }
  return temp_reg;
}

static const char *parse_factor(Token *tokens, int count, int *idx, FILE *out) {
  if (*idx >= count - 1)
    return NULL;

  Token *t = &tokens[*idx];

  if (t->type == TOKEN_DELIMITER && strcmp(t->value, "(") == 0) {
    (*idx)++;
    const char *r = parse_expression(tokens, count, idx, out);
    if (*idx < count - 1 && tokens[*idx].type == TOKEN_DELIMITER &&
        strcmp(tokens[*idx].value, ")") == 0) {
      (*idx)++;
      return r;
    } else {
      return NULL;
    }
  } else if (t->type == TOKEN_INTEGER || t->type == TOKEN_VARIABLE ||
             t->type == TOKEN_CHAR) {
    const char *r = load_operand(t, out);
    (*idx)++;
    return r;
  } else {
    return NULL;
  }
}

static const char *parse_term(Token *tokens, int count, int *idx, FILE *out) {
  const char *left_reg = parse_factor(tokens, count, idx, out);
  if (!left_reg)
    return NULL;

  while (*idx < count - 1 && tokens[*idx].type == TOKEN_OPERATOR &&
         (strcmp(tokens[*idx].value, "*") == 0 ||
          strcmp(tokens[*idx].value, "/") == 0)) {
    char op = tokens[*idx].value[0];
    (*idx)++;

    const char *right_reg = parse_factor(tokens, count, idx, out);
    if (!right_reg)
      return NULL;

    char comment[128];
    int left_num = atoi(left_reg + 1);
    int right_num = atoi(right_reg + 1);

    if (op == '*') {
      snprintf(comment, sizeof(comment), "[ %s * %s ]", left_reg, right_reg);
      fprintf(out, "DMULTU %s, %s\n", left_reg, right_reg);
      write_machine_code(out, "DMULTU", 0, left_num, right_num, 0, comment);
      snprintf(comment, sizeof(comment), "[ MFLO %s ]", left_reg);
      fprintf(out, "MFLO %s\n", left_reg);
      fprintf(
          out,
          ">> No associated MFLO assembly and machine code in the reference\n");
      // write_machine_code(out, "MFLO", left_num, 0, 0, 0, comment);
    } else {
      snprintf(comment, sizeof(comment), "[ %s / %s ]", left_reg, right_reg);
      fprintf(out, "DDIVU %s, %s\n", left_reg, right_reg);
      write_machine_code(out, "DDIVU", 0, left_num, right_num, 0, comment);
      snprintf(comment, sizeof(comment), "[ MFLO %s ]", left_reg);
      fprintf(out, "MFLO %s\n", left_reg);
      fprintf(
          out,
          ">> No associated MFLO assembly and machine code in the reference\n");
      // write_machine_code(out, "MFLO", left_num, 0, 0, 0, comment);
    }
  }

  return left_reg;
}

static const char *parse_expression(Token *tokens, int count, int *idx,
                                    FILE *out) {
  const char *left_reg = parse_term(tokens, count, idx, out);
  if (!left_reg)
    return NULL;

  while (*idx < count - 1 && tokens[*idx].type == TOKEN_OPERATOR &&
         (strcmp(tokens[*idx].value, "+") == 0 ||
          strcmp(tokens[*idx].value, "-") == 0)) {
    char op = tokens[*idx].value[0];
    (*idx)++;

    const char *right_reg = parse_term(tokens, count, idx, out);
    if (!right_reg)
      return NULL;

    char comment[128];
    int left_num = atoi(left_reg + 1);
    int right_num = atoi(right_reg + 1);

    if (op == '+') {
      snprintf(comment, sizeof(comment), "[ %s + %s ]", left_reg, right_reg);
      fprintf(out, "DADDU %s, %s, %s\n", left_reg, left_reg, right_reg);
      write_machine_code(out, "DADDU", left_num, left_num, right_num, 0,
                         comment);
    } else {
      snprintf(comment, sizeof(comment), "[ %s - %s ]", left_reg, right_reg);
      fprintf(out, "DSUBU %s, %s, %s\n", left_reg, left_reg, right_reg);
      write_machine_code(out, "DSUBU", left_num, left_num, right_num, 0,
                         comment);
    }
  }

  return left_reg;
}

// Translation Functions
static void store_to_var(FILE *out, const char *src_reg, const char *var_name) {
  int sym_idx = find_symbol(var_name);
  if (sym_idx < 0)
    return;
  int offset = symbol_table[sym_idx].addr;
  int src_num = atoi(src_reg + 1);
  char comment[128];
  snprintf(comment, sizeof(comment), "[ store %s to %s ]", src_reg, var_name);
  fprintf(out, "SD %s, %s(R0)\n", src_reg, var_name);
  write_machine_code(out, "SD", 0, 0, src_num, offset, comment);
}

static void translate_multi_declaration(Token *tokens, int count, FILE *out) {
  // Find '=' position if present
  int eq_index = -1;
  for (int i = 1; i < count - 1; i++) {
    if (tokens[i].type == TOKEN_OPERATOR && strcmp(tokens[i].value, "=") == 0) {
      eq_index = i;
      break;
    }
  }

  // If no init, print message and return (.data handles declaration)
  if (eq_index == -1) {
    fprintf(out, "Declarations are handled on the .data section\n");
    return;
  }

  // Target var is the one before =
  if (eq_index - 1 < 1 || tokens[eq_index - 1].type != TOKEN_VARIABLE)
    return;
  const char *target_var = tokens[eq_index - 1].value;

  // Parse and compute expression
  temp_count = 0;
  int idx = eq_index + 1;
  const char *expr_reg = parse_expression(tokens, count, &idx, out);
  if (!expr_reg)
    return;

  // Store result
  store_to_var(out, expr_reg, target_var);
}

static void translate_assignment(Token *tokens, int count, FILE *out) {
  const char *target_var = tokens[0].value;
  Token *src_token = &tokens[2];
  temp_count = 0;
  const char *temp_reg = load_operand(src_token, out);
  if (!temp_reg)
    return;
  store_to_var(out, temp_reg, target_var);
}

static void translate_arithmetic(Token *tokens, int count, FILE *out) {
  const char *target_var = tokens[0].value;
  temp_count = 0;
  int idx = 2;
  const char *expr_reg = parse_expression(tokens, count, &idx, out);
  if (!expr_reg)
    return;
  store_to_var(out, expr_reg, target_var);
}


static void translate_to_assembly(Token *tokens, int count, FILE *out) {
    int pos = 0;

    while (pos < count) {
        // Skip semicolons
        while (pos < count && tokens[pos].type == TOKEN_DELIMITER &&
               strcmp(tokens[pos].value, ";") == 0) {
            pos++;
        }
        if (pos >= count) break;

        // Find end of current statement
        int stmt_start = pos;
        int stmt_end = pos;
        while (stmt_end < count) {
            if (tokens[stmt_end].type == TOKEN_DELIMITER &&
                strcmp(tokens[stmt_end].value, ";") == 0) {
                stmt_end++;
                break;
            }
            stmt_end++;
        }
        if (stmt_end > count) stmt_end = count;

        int stmt_len = stmt_end - stmt_start;
        if (stmt_len == 0) break;

        Token *stmt = tokens + stmt_start;

        // Now translate this one statement
        if (stmt[0].type == TOKEN_KEYWORD) {
            translate_multi_declaration(stmt, stmt_len, out);
        }
        else if (stmt_len == 4 && stmt[0].type == TOKEN_VARIABLE) {
            translate_assignment(stmt, stmt_len, out);
        }
        else if (stmt_len >= 5 && stmt[0].type == TOKEN_VARIABLE) {
            translate_arithmetic(stmt, stmt_len, out);
        }

        pos = stmt_end;
    }
}

// =========================
// Main
// =========================
int main() {
  FILE *in = fopen("assembly.txt", "r");
  FILE *out = fopen("output.txt", "w");
  static ValidLine valid_lines[MAX_VALID_LINES];
  int valid_count = 0;

  if (!in) {
    printf("Error: assembly.txt not found.\n");
    return 1;
  }

  if (!out) {
    printf("Error: Cannot create output.txt\n");
    fclose(in);
    return 1;
  }

  char line[512];
  Token tokens[MAX_TOKENS];
  int line_no = 1;
  int total_lines = 0;
  int correct_lines = 0;
  int has_error = 0; //  Flag to track if any error occurred

  printf("=== Syntax & Semantic Analyzer with Assembly Translator ===\n\n");
  fprintf(out, "# Assembly code generated by compiler\n");

  // First pass: validate and collect (breaks on first error)
  while (fgets(line, sizeof(line), in)) {
    line[strcspn(line, "\n")] = '\0';

    if (strlen(line) == 0) {
      line_no++;
      continue;
    }

    total_lines++;
    // int x example
    int count = tokenize(line, tokens, MAX_TOKENS);

    // Skip lines that are only semicolons
    int only_semicolons = 1;
    for (int i = 0; i < count; i++) {
        if (tokens[i].type != TOKEN_DELIMITER || strcmp(tokens[i].value, ";") != 0) {
            only_semicolons = 0;
            break;
        }
    }
    if (only_semicolons && count > 0) {
        printf("line [%d]: empty statement(s) → skipped\n\n", line_no);
        line_no++;
        continue;
    }

    if (count == 0) {
      line_no++;
      continue;
    }

    printf("line [%d]: %s\n", line_no, line);

    // First: Check Syntax
    int syntax_valid = validate_syntax(tokens, count, line_no, out);
    if (!syntax_valid) {
      printf("     SYNTAX ERROR at line %d\n\n", line_no);
      has_error = 1; // Set flag
      break;         // Stops processing further lines
    }
    ///////////////////
    // Second: Check Semantics (only if syntax is correct)
    else {
        int pos = 0;
        int semantic_ok = 1;

        while (pos < count && semantic_ok) {
            while (pos < count && tokens[pos].type == TOKEN_DELIMITER &&
                   strcmp(tokens[pos].value, ";") == 0) {
                pos++;
            }
            if (pos >= count) break;

            int stmt_start = pos;
            int stmt_end = pos;
            while (stmt_end < count) {
                if (tokens[stmt_end].type == TOKEN_DELIMITER &&
                    strcmp(tokens[stmt_end].value, ";") == 0) {
                    stmt_end++;
                    break;
                }
                stmt_end++;
            }
            if (stmt_end > count) stmt_end = count;
            int stmt_len = stmt_end - stmt_start;
            if (stmt_len == 0) break;

            Token *stmt = tokens + stmt_start;

            if (!process_one_statement_semantically(stmt, stmt_len, line_no)) {
                printf("     SEMANTIC ERROR at line %d\n\n", line_no);
                semantic_ok = 0;
            }

            pos = stmt_end;
        }

        if (!semantic_ok) {
            has_error = 1;
            break;
        }

        printf("     SYNTAX CORRECT\n\n");

        if (valid_count < MAX_VALID_LINES) {
            strcpy(valid_lines[valid_count].line, line);
            memcpy(valid_lines[valid_count].tokens, tokens, sizeof(Token) * count);
            valid_lines[valid_count].count = count;
            valid_lines[valid_count].line_no = line_no;
            valid_count++;
        } 
        correct_lines++;
    }

    line_no++;
  }

  fclose(in);

  // Skip generation if any error flagged (even if partial valid lines)
  if (has_error) {
    printf(
        "Error encountered - skipping assembly and machine code generation.\n");
    fprintf(out, "# Error encountered - no code generated\n");
  } else {
    // Write .data section
    fprintf(out, ".data\n");
    int current_addr = 0;
    for (int i = 0; i < symbol_count; i++) {
      symbol_table[i].addr = current_addr;
      current_addr += 8;
      fprintf(out, "%s: .dword    # 0x%08X\n", symbol_table[i].var,
              symbol_table[i].addr);
    }
    fprintf(out, "\n.code\n\n");

    // Second pass: Translate valid lines
    for (int v = 0; v < valid_count; v++) {
      ValidLine *vl = &valid_lines[v];
      fprintf(out, "line [%d]: %s\n", vl->line_no, vl->line);
      fprintf(out, "==========================================================="
                   "=====================\n\n");
      translate_to_assembly(vl->tokens, vl->count, out);
      fprintf(out, "\n========================================================="
                   "=======================\n\n");
    }
  }

  // Print summary (based on processed lines only)
  printf("=== Summary ===\n");
  printf("Total lines: %d\n", total_lines);
  printf("Correct: %d\n", correct_lines);
  printf("Errors: %d\n", total_lines - correct_lines);
  printf("\nMachine and assembly code generated\n"); 

  fprintf(out, "\n# Summary\n");
  fprintf(out, "# Total lines: %d\n", total_lines);
  fprintf(out, "# Correct: %d\n", correct_lines);
  fprintf(out, "# Errors: %d\n", total_lines - correct_lines);

  fclose(out);

  return 0;
}
 