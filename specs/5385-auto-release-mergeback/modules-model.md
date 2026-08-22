# Modules model

| ID | Component | Responsibility | Depends on |
| --- | --- | --- | --- |
| M1 | Publish Release workflow | Invoke merge-back creation for eligible releases with scoped authority | M2 |
| M2 | Merge-back operation | Validate release identity, detect an existing PR, and request creation when absent | Git/GitHub public metadata |
| M3 | Regression proof | Exercise M2 decisions without GitHub mutation | M2 |
