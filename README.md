# Demonstration of model serving

This provides demonstrations for model serving and prototypes.
The server is on heroku, so anyone can try it.

There are several model serving patterns that we support here.
https://github.com/hasktorch/hasktorch/tree/master/examples/model-serving

The following command works on heroku. The first time is slow, because it stops every 30 minutes.

```
$ curl -s https://hasktorch-serving-models.herokuapp.com/compute2x/3 | jq .
[
  {
    "result": [
      6
    ],
    "msg": "f(x) = 2.0 * x is 6.0 for x = 3.0"
  }
]

$ curl -o output.jpg -F image="@street.jpg" -F labels="@bdd100k.names" -F bbox="@street.txt" http://hasktorch-serving-models.herokuapp.com/boundingbox
```

# Setup

1. Login heroku and create a new application on heroku.
2. Write heroku.yml
3. Run `heroku stack:set container` to use docker for deploy.
4. Push the codes on github.

For now, the executable file for model serving is from github-release.

https://github.com/hasktorch/hasktorch-serving-models-skeleton/releases

# References

* https://devcenter.heroku.com/articles/build-docker-images-heroku-yml#getting-started
