#https://reformatcode.com/code/git/automatic-merge-branch-into-master-on-sucessful-build-in-travis

echo "merge_script.sh"
if [ "$TRAVIS_BRANCH" == "master"]; then
    echo "merge into production"

    git checkout production || exit
    git merge "$TRAVIS_COMMIT" || exit
    git push origin production || exit
fi

if [ "$TRAVIS_BRANCH" == "production"]; then
    echo "CI build of production branch successful"
fi
