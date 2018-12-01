# aws-lambda-haskell

This package makes it easy to run AWS Lambda Functions written in Haskell.

## Example function

Examples can be found in the examples directory.

In order to build them you'll need to fetch the `fpco/stack-build:lts-12.14` docker image. The run `stack build` in the root directory.

## Deployment

- Build the executable (you'll need to target your executable for an x86_64-linux platform). Compilation is that case is done through stack but any other tool would be fine. 

It's advised to build the executable without the --threaded flag. AWS lambdas run in a single thread.


```bash
stack build
```

- Create a zip file with the executable file. The file name must be "bootstrap".

```bash
cp $(stack exec -- which aws-lambda-haskell-simple-example-exe) ./bootstrap && zip demo.zip bootstrap && rm bootstrap
```

- Create an execution role for the lambda

```
aws iam create-role --role-name lambda-demo --assume-role-policy-document file://./examples/simple/trust-policy.json
aws iam attach-role-policy --role-name lambda-demo --policy-arn arn:aws:iam::aws:policy/service-role/AWSLambdaBasicExecutionRole
```

- Create the lambda

```
aws lambda create-function --function-name demo \
--role <the-arn-of-the-role-that-was-created-in-the-previous-step> \
--runtime provided --timeout 15 --memory-size 128 \
--handler demo --zip-file fileb://demo.zip
```

- Invoke it

```
aws lambda invoke --function-name demo --payload '{"answer":42}' output.txt
```

## Credits

This package is heavily inspired in the runtime for [rust](https://github.com/awslabs/aws-lambda-rust-runtime) and [cpp](https://github.com/awslabs/aws-lambda-cpp)
