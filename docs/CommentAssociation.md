# Comment Association in SSL Formatter

This document describes the comment association functionality implemented in the SSL formatter, which handles the proper positioning and preservation of comments during code formatting.

## Overview

The comment association system automatically determines relationships between comments and AST nodes, ensuring that comments are properly maintained in their intended positions relative to the code they document.

## Features

### Comment Types Supported

According to the SSL EBNF grammar, the following comment types are supported:

-   **Block Comments** (`/* comment ;`) - Multi-line comments
-   **Single-line Comments** (`/* comment ;`) - Comments on a single line
-   **Region Comments** (`/* region name ;`) - Code organization markers
-   **End Region Comments** (`/* endregion name ;`) - Region closing markers

### Association Types

Comments are associated with AST nodes using the following relationships:

-   **Leading Comments** - Comments that appear before an AST node (typically 1-2 lines above)
-   **Trailing Comments** - Comments that appear on the same line as an AST node, after the code
-   **Following Comments** - Comments that appear after an AST node (typically 1 line below)
-   **Standalone Comments** - Comments that are not closely associated with any specific AST node

## Implementation

### Core Classes

#### `CommentAssociator`

The main class that handles comment-to-node associations:

```typescript
import {
    CommentAssociator,
    defaultCommentAssociationOptions,
} from "./formatter/commentAssociation";

const associator = new CommentAssociator(defaultCommentAssociationOptions);
const associations = associator.associateComments(commentNodes, astNodes);
```

#### `CommentAssociation`

Represents a single association between a comment and an AST node:

```typescript
interface CommentAssociation {
    comment: CommentNode;
    associatedNode: ASTNode | null;
    position: CommentPosition;
    lineNumber: number;
    columnPosition: number;
    preserve: boolean;
}
```

#### `CommentPosition`

Enum defining the relationship between a comment and its associated node:

```typescript
enum CommentPosition {
    Leading = "Leading", // Comment before the node
    Trailing = "Trailing", // Comment after node on same line
    Following = "Following", // Comment after node on different line
    Standalone = "Standalone", // Comment not associated with any node
}
```

### Configuration Options

```typescript
interface CommentAssociationOptions {
    maxLeadingDistance: number; // Max lines before a node for leading comments (default: 2)
    maxFollowingDistance: number; // Max lines after a node for following comments (default: 1)
    associateWithClosestNode: boolean; // Associate with closest node (default: true)
    preserveStandaloneComments: boolean; // Keep standalone comments (default: true)
    associateRegionComments: boolean; // Special handling for region comments (default: true)
}
```

## Usage Examples

### Basic Comment Association

```typescript
import { CommentAssociator } from "./formatter/commentAssociation";

// Create associator
const associator = new CommentAssociator();

// Associate comments with AST nodes
const associations = associator.associateComments(commentNodes, astNodes);

// Get leading comments for a specific node
const leadingComments = associator.getLeadingComments(procedureNode);

// Get trailing comments for a specific node
const trailingComments = associator.getTrailingComments(declarationNode);

// Get standalone comments
const standaloneComments = associator.getStandaloneComments();
```

### Integration with SSL Formatter

The comment association system is automatically integrated into the main `SSLFormatter`:

```typescript
import { SSLFormatter } from "./formatter";

const formatter = new SSLFormatter();
const formattedCode = formatter.format(ast); // Comments are automatically associated and preserved
```

### Custom Configuration

```typescript
import { CommentAssociator, CommentAssociationOptions } from "./formatter/commentAssociation";

const customOptions: CommentAssociationOptions = {
    maxLeadingDistance: 3, // Allow comments up to 3 lines before
    maxFollowingDistance: 2, // Allow comments up to 2 lines after
    associateWithClosestNode: true,
    preserveStandaloneComments: true,
    associateRegionComments: true,
};

const associator = new CommentAssociator(customOptions);
```

## SSL EBNF Grammar Compliance

The comment association system fully complies with the SSL EBNF grammar definitions:

### Comment Syntax

-   Comments must start with `/*` and end with `;`
-   Block comments can span multiple lines
-   Region comments follow the pattern `/* region name ;`
-   End region comments follow the pattern `/* endregion name ;`

### Positioning Rules

-   Leading comments are associated with the next significant code structure
-   Trailing comments remain on the same line as their associated code
-   Region comments are specially associated with block structures (procedures, classes, control flow)
-   Standalone comments are preserved in their original position

## Examples

### Leading Comment Association

```ssl
/* This procedure validates user input data ;
/* It checks for required fields and data types ;
:PROCEDURE ValidateUserInput;
    :PARAMETERS sUserName, nAge;
    /* Procedure implementation ;
:ENDPROC;
```

**Result**: Both comments are associated as leading comments with the `ValidateUserInput` procedure.

### Trailing Comment Association

```ssl
:DECLARE sUserName;     /* Store the user's name ;
:DECLARE nUserAge;      /* Store the user's age ;
```

**Result**: Each comment is associated as a trailing comment with its respective declaration.

### Region Comment Association

```ssl
/* region Data Validation Functions ;

:PROCEDURE ValidateEmail;
    /* Email validation logic ;
:ENDPROC;

:PROCEDURE ValidatePhone;
    /* Phone validation logic ;
:ENDPROC;

/* endregion Data Validation Functions ;
```

**Result**: The region comment is associated with the first procedure as a leading comment, helping to organize code sections.

### Standalone Comment Preservation

```ssl
:PROCEDURE ProcessData;
    /* Main processing logic ;
:ENDPROC;

/* TODO: Add error handling to all procedures
   This is a reminder for future development ;

:PROCEDURE SaveData;
    /* Save logic ;
:ENDPROC;
```

**Result**: The TODO comment is preserved as a standalone comment since it's not closely associated with either procedure.

## Benefits

1. **Comment Preservation** - Ensures all comments are maintained during formatting
2. **Intelligent Positioning** - Comments stay logically positioned relative to the code they document
3. **EBNF Compliance** - Follows SSL language specifications exactly
4. **Flexible Configuration** - Customizable association rules for different coding standards
5. **Integration Ready** - Seamlessly works with the existing formatter infrastructure

## Testing

The comment association functionality includes comprehensive tests:

-   **Unit Tests** (`commentAssociation.test.ts`) - Test individual association logic
-   **Integration Tests** (`commentAssociationIntegration.test.ts`) - Test integration with main formatter
-   **EBNF Compliance Tests** - Verify adherence to SSL grammar specifications

Run tests with:

```bash
npm test -- test/formatter/commentAssociation.test.ts
npm test -- test/formatter/commentAssociationIntegration.test.ts
```

## Implementation Notes

### Performance Considerations

-   Comments and nodes are sorted once for efficient processing
-   Association lookups use optimized distance calculations
-   Memory usage is minimized through efficient data structures

### Future Enhancements

-   Support for custom comment patterns
-   Advanced region matching algorithms
-   Integration with language server protocol for real-time association
-   Support for nested comment structures

### Compatibility

-   Fully compatible with existing SSL formatter functionality
-   No breaking changes to existing APIs
-   Backward compatible with all SSL language versions
