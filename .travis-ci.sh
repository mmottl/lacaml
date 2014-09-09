echo "yes" | sudo add-apt-repository ppa:avsm/ppa
sudo apt-get update -qq
sudo apt-get install ocaml ocaml-native-compilers opam liblapack-dev

export OPAMYES=1
opam init
opam install oasis ocamlfind
eval `opam config env`

ocaml setup.ml -configure
ocaml setup.ml -build
ocaml setup.ml -install

if [ "$TRAVIS_REPO_SLUG" = "mmottl/lacaml" ] \
    && [ "$TRAVIS_PULL_REQUEST" = "false" ] \
    && [ "$TRAVIS_BRANCH" = "master" ] \
    && [ -n "$TRAVIS_TAG" ]; then

    echo "Publishing documentation ($TRAVIS_TAG)..."

    ocaml setup.ml -doc

    git config --global user.email "travis@travis-ci.org"
    git config --global user.name "travis-ci"
    git clone https://github.com/mmottl/lacaml.git documentation

    cd documentation
    git checkout gh-pages
    cp -a ../_build/API.docdir/ api

    git add --force api

  if [ -n "$(git status --untracked-files=no --porcelain)" ]; then
    git commit -m "Update documentation $TRAVIS_TAG"
    git push --force --quiet origin gh-pages
  fi

  echo "Done (Lacaml documentation for $TRAVIS_TAG)."
fi
