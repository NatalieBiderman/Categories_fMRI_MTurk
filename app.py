from flask import Flask, render_template, request
from flask import redirect, url_for
from manage_subject_info import *
from mturk_utils import *
from io import StringIO
import pandas as pd

_thisDir = os.path.dirname(os.path.abspath(__file__))

app = Flask(__name__)

#@app.route('/')
#def hello_world():
#    return 'Hello from Flask'

@app.route('/', methods=["GET","POST"])
@app.route('/category_form', methods=["GET","POST"])
def category_form():
    if request.method == "GET":
        return render_template('category_form.html')
    else:
        if request.form['is_scanner'] == 'mturk':
            return redirect(url_for('consent'))
        else:
            is_scanner = request.form['is_scanner']
            run_category_learning = request.form['run_category_learning']
            run_category_memory = request.form['run_category_memory']
            start_t = request.form['start_t']
            end_t = request.form['end_t']
            start_t_category_memory = request.form['start_t_category_memory']
            end_t_category_memory = request.form['end_t_category_memory']
            subID = request.form['subID']
            group = request.form['group']
            age = request.form['age']
            if not os.path.exists(os.path.join('data','Subj_Info')):
                os.mkdir(os.path.join('data','Subj_Info'))
            data = pd.DataFrame.from_dict({"subID":[subID],"group":[group],"age":[age]})
            data.to_csv(os.path.join('data','Subj_Info',"subj_info_sub_%s.csv"%subID), index=False)
            return redirect(url_for('category_learning',
                                    is_scanner=is_scanner,
                                    run_category_learning=run_category_learning,
                                    run_category_memory=run_category_memory,
                                    start_t=start_t,
                                    end_t=end_t,
                                    start_t_category_memory=start_t_category_memory,
                                    end_t_category_memory=end_t_category_memory,
                                    subID=subID))

@app.route('/consent')
def consent():
    return render_template('Category_learning/consent.html')

@app.route('/post_data', methods=["POST"])
def post_data():
    if request.is_json:
        ## Retrieve jsPsych data.
        data = request.get_json()
        filepath = data['filename']
        save_data(filepath, StringIO(data['data']))
    return ('', 200)

@app.route('/category_index')
def category_index():
    return render_template('Category_learning/index.html')

@app.route('/category_learning', methods=["GET","POST"])
def category_learning():
    is_scanner = request.args.get('is_scanner', default='scanner')
    run_category_learning = request.args.get('run_category_learning', default=1, type=int)
    run_category_memory = request.args.get('run_category_memory', default=1, type=int)
    start_t = request.args.get('start_t', default=0, type=int)
    end_t = request.args.get('end_t', default=364, type=int)
    start_t_category_memory = request.args.get('start_t_category_memory', default=0, type=int)
    end_t_category_memory = request.args.get('end_t_category_memory', default=35, type=int)
    subID = request.args.get('subID', default=1, type=int)

    if request.method == "GET":
        return render_template('Category_learning/start_category_learning_exp.html',
                                        is_scanner=is_scanner,
                                        run_category_learning=run_category_learning,
                                        run_category_memory=run_category_memory,
                                        start_t=start_t,
                                        end_t=end_t,
                                        start_t_category_memory=start_t_category_memory,
                                        end_t_category_memory=end_t_category_memory,
                                        subID=subID)
    else:
        if request.is_json:
            ## Retrieve jsPsych data.
            data = request.get_json()
            filepath = data['filename']
            save_data(filepath, StringIO(data['data']))
        return ('', 200)

@app.route('/rl_form', methods=["GET","POST"])
def rl_form():
    if request.method == "GET":
        return render_template('rl_form.html')
    else:
        if request.form['is_scanner'] == 'mturk':
            return redirect(url_for('consent'))
        else:
            is_scanner = request.form['is_scanner']
            start_t = request.form['start_t']
            end_t = request.form['end_t']
            subID = request.form['subID']
            return redirect(url_for('simple_rl',
                                    is_scanner=is_scanner,
                                    start_t=start_t,
                                    end_t=end_t,
                                    subID=subID))

@app.route('/simple_rl', methods=["GET","POST"])
def simple_rl():
    is_scanner = request.args.get('is_scanner', default='scanner')
    start_t = request.args.get('start_t', default=0, type=int)
    end_t = request.args.get('end_t', default=79, type=int)
    subID = request.args.get('subID', default=1, type=int)

    if request.method == "GET":
        return render_template('Simple_RL/start_RL_exp.html',
                           is_scanner=is_scanner,
                           start_t=start_t,
                           end_t=end_t,
                           subID=subID)
    else:
        if request.is_json:
            ## Retrieve jsPsych data.
            data = request.get_json()
            filepath = data['filename']
            save_data(filepath, StringIO(data['data']))
        return ('', 200)

@app.route('/size_form', methods=["GET","POST"])
def size_form():
    if request.method == "GET":
        return render_template('size_form.html')
    else:
        if request.form['is_scanner'] == 'mturk':
            return redirect(url_for('consent'))
        else:
            is_scanner = request.form['is_scanner']
            start_t = request.form['start_t']
            end_t = request.form['end_t']
            subID = request.form['subID']

            return redirect(url_for('size_judgement',
                                    is_scanner=is_scanner,
                                    start_t=start_t,
                                    end_t=end_t,
                                    subID=subID))

@app.route('/size_judgement', methods=["GET","POST"])
def size_judgement():
    if request.method == "GET":
        is_scanner = request.args.get('is_scanner', default='scanner')
        start_t = request.args.get('start_t', default=0, type=int)
        end_t = request.args.get('end_t', default=89, type=int)
        subID = request.args.get('subID', default=1, type=int)
        return render_template('Size_judgement/start_size_exp.html',
                               is_scanner=is_scanner,
                               start_t=start_t,
                               end_t=end_t,
                               subID=subID)
    else:
        if request.is_json:
            ## Retrieve jsPsych data.
            data = request.get_json()
            filepath = data['filename']
            save_data(filepath, StringIO(data['data']))
        return ('', 200)


@app.route('/thankyou')
def thankyou():
    return render_template('thankyou.html')

@app.route("/unauthorized_error", methods=["GET"])
def unauthorized_error():
    return render_template('unauthorized_error.html')


@app.route("/page_not_found", methods=["GET"])
def page_not_found():
    return render_template('404.html')


@app.errorhandler(404)
def page_not_found(e):
    return render_template('404.html'), 404


@app.errorhandler(500)
def internal_server_error(e):
    return render_template('500.html'), 500


if __name__ == "__main__":
    app.debug = True
    app.config['SEND_FILE_MAX_AGE_DEFAULT'] = 0
    app.run(host='0.0.0.0', port=8000)
