pipeline {
    agent none
    options { disableConcurrentBuilds() }
    environment {
        CUR_PROJ = 'mpi-plotr' // github repo name
        CUR_PKG = 'PlotR' // r-package name
        CUR_PKG_FOLDER = '.' // defaults to root
        INWT_REPO = 'inwt-vmdocker1.inwt.de:8081'
    }
    stages {
        stage('Testing with R') {
            agent { label 'docker1' }
            when {
                not { branch 'staging' }
                not { branch 'production'}
            }
            environment {
                TMP_SUFFIX = """${sh(returnStdout: true, script: 'echo `cat /dev/urandom | tr -dc \'a-z\' | fold -w 6 | head -n 1`')}"""
            }
            steps {
                sh '''
                docker build --pull -t tmp-$CUR_PROJ-$TMP_SUFFIX .
                docker run --rm --network host tmp-$CUR_PROJ-$TMP_SUFFIX check
                docker rmi tmp-$CUR_PROJ-$TMP_SUFFIX
                '''
            }
        }

        stage('Deploy R-package') {
            agent { label 'eh2' }
            when { branch 'main' }
            steps {
                sh '''
                rm -vf *.tar.gz
                docker pull inwt/r-batch:latest
                docker run --rm --network host -v $PWD:/app --user `id -u`:`id -g` inwt/r-batch:latest R CMD build $CUR_PKG_FOLDER
                docker run --rm -v $PWD:/app -v /var/www/html/r-repo:/var/www/html/r-repo inwt/r-batch:latest R -e "drat::insertPackage(dir(pattern='.tar.gz'), '/var/www/html/r-repo'); drat::archivePackages(repopath = '/var/www/html/r-repo')"
                '''
            }
        }

        stage('Deploy Docker-image') {
            agent { label 'docker1' }
            when  { branch 'main' }
            steps {
                sh '''
                docker build --pull -t $INWT_REPO/$CUR_PROJ:latest .
                docker push $INWT_REPO/$CUR_PROJ:latest
                docker rmi $INWT_REPO/$CUR_PROJ:latest
                '''
            }
        }

        stage('Pull Image on EH2') {
            agent { label 'eh2' }
            when { branch 'main' }
            steps {
                sh '''
                docker pull $INWT_REPO/$CUR_PROJ:latest
                '''
            }
        }
    }
    post {
        always {
            script {
                if (env.BRANCH_NAME != 'main' && env.BRANCH_NAME != 'staging' && env.BRANCH_NAME != 'production') {
                    emailext (
                        attachLog: true,
                        body: "Build of job ${env.JOB_NAME} (No. ${env.BUILD_NUMBER}) has completed\n\nBuild status: ${currentBuild.currentResult}\n\n${env.BUILD_URL}\n\nSee attached log file for more details of the build process.",
                        recipientProviders: [[$class: 'DevelopersRecipientProvider'], [$class: 'RequesterRecipientProvider']],
                        subject: "Jenkins Build ${currentBuild.currentResult}: Job ${env.JOB_NAME}"
                    )
                }
            }
        }
        failure {
           script {
                if (env.BRANCH_NAME == 'main') {
                    emailext (
                        attachLog: true,
                        body: "Build of job ${env.JOB_NAME} (No. ${env.BUILD_NUMBER}) has completed\n\nBuild status: ${currentBuild.currentResult}\n\n${env.BUILD_URL}\n\nSee attached log file for more details of the build process.",
                        recipientProviders: [[$class: 'DevelopersRecipientProvider'], [$class: 'RequesterRecipientProvider']],
                        to: "andreas.neudecker@inwt-statistics.de,marcus.gross@inwt-statistics.de",
                        subject: "Jenkins Build ${currentBuild.currentResult}: Job ${env.JOB_NAME}"
                    )
                }
            }
        }
    }
}